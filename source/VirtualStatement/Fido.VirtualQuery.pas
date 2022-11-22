(*
 * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

unit Fido.VirtualQuery;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.Classes,
  System.StrUtils,
  System.SysUtils,
  System.Variants,
  System.Generics.Collections,
  Data.DB,

  Spring,
  Spring.Container,
  Spring.Collections,
  Spring.Collections.Base,

  Fido.Utilities,
  Fido.Exceptions,
  Fido.Db.TypeConverter,
  Fido.Resource.StringReader.Intf,
  Fido.VirtualStatement.Attributes,
  Fido.StatementExecutor.Intf,
  Fido.VirtualInterface,
  Fido.VirtualDto.Database,
  Fido.Types,
  Fido.VirtualQuery.Intf,
  Fido.VirtualQuery.Metadata.Intf,
  Fido.Virtual.Attributes,
  Fido.VirtualQuery.Attributes,
  Fido.DesignPatterns.Adapter.DataSetAsReadonlyList;

type
  EFidoVirtualQueryError = class(EFidoException);

  TVirtualQuery<TRecord: IInterface; T: IVirtualQuery> = class(TVirtualInterface<T>, IVirtualQueryMetadata)
  strict private const
    GetterPrefix = 'GET';
    DatasetOperationNames: array [TDatasetOperation] of string = ('', 'NEXT', 'FIRST', 'CLOSE', 'GETEOF');
  strict private
    FDescription: string;
    FSQLResource: string;
    FResourcedSQL: string;
    FParameterCommaList: string;
    FExecutor: IStatementExecutor;
    FExecMethod: TMethodDescriptor;
    FDataset: TDataset;
    FList: Weak<IReadonlyList<TRecord>>;
    FParams: IDictionary<string, TParamDescriptor>;
    FMethods: IDictionary<string, TMethodDescriptor>;

    function AddOrUpdateDescriptor(const OriginalName: string; const RttiType: TRttiType; const Direction: TParamType; const IsPagingLimit: Boolean; const IsPagingOffset: Boolean;
      const SqlInjectTag: string): TParamDescriptor;
    procedure ProcessAllAttributes;
    function ExtractSQLString(const ResString: string): string;
    function GetSQLData: string;
    function GetIsDefined: boolean;
    function GetIsGetterName(const Name: string): boolean;
    function GetMappedName(const Name: string): string;
    procedure SetEnumeratorValue(out Result: TValue);
    procedure Execute(const Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
    procedure ProcessAttribute(const Attribute: TCustomAttribute; const Method: TRttiMethod = nil; const MethDesc: TMethodDescriptor = nil);
    procedure ProcessMethod(const Method: TRttiMethod; const Collection: IDictionary<string, TMethodDescriptor>);
    procedure RaiseError(const Msg: string; const Args: array of const);
    procedure TestDatasetOpen(const MethodToBeCalled: string);
    procedure SetExecMethod(const Value: TMethodDescriptor);
    procedure DefineStatement(const Method: TRttiMethod;const Args: TArray<TValue>);
    procedure ValidateStatement;
    function ReplaceSqlInject(const Sql: string; const Args: TArray<TValue>): string;
  private
    procedure DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);

    property Executor: IStatementExecutor read FExecutor;
    property ExecMethod: TMethodDescriptor read FExecMethod write SetExecMethod;
  public
    constructor Create(const ResReader: IStringResourceReader; const StatementExecutor: IStatementExecutor);

    class function GetInstance(const Container: TContainer; const StatementExecutorServiceName: string = ''): TVirtualQuery<TRecord, T>; static;
    // IVirtualQueryMetadata
    function GetDescription: string;
    function GetSQLResource: string;

    property SQLResource: string read GetSQLResource;
  end;

implementation

{ TVirtualStatement<TRecord, T> }

function TVirtualQuery<TRecord, T>.AddOrUpdateDescriptor(
  const OriginalName: string;
  const RttiType: TRttiType;
  const Direction: TParamType;
  const IsPagingLimit: Boolean;
  const IsPagingOffset: Boolean;
  const SqlInjectTag: string): TParamDescriptor;
var
  MappedName: string;
begin
  MappedName := GetMappedName(OriginalName);

  if FParams.TryGetValue(MappedName, Result) then
  begin
    if (Direction = ptOutput) and (Result.Direction = ptInput) then
      Result.Direction := ptInputOutput;
    // these must not change
    Assert(Result.DataType = DataTypeConverter.GetDescriptor(RttiType));
    Exit;
  end
  else begin
    Result := TParamDescriptor.Create;
    FParams.Add(MappedName, Result);
    Result.Index := FParams.Count;
    Result.MappedName := MappedName;
    Result.DataType := DataTypeConverter.GetDescriptor(RttiType);
    Result.Direction := Direction;
    Result.IsPagingLimit := IsPagingLimit;
    Result.IsPagingOffset := IsPagingOffset;
    Result.SqlInjectTag := SqlInjectTag;
    if Direction in [ptInput, ptInputOutput] then
      FParameterCommaList := FParameterCommaList + ', :' + MappedName;
  end;
end;

constructor TVirtualQuery<TRecord, T>.Create(
  const ResReader: IStringResourceReader;
  const StatementExecutor: IStatementExecutor);
begin
  Guard.CheckNotNull(ResReader, 'ResReader');
  inherited Create(DoInvoke);

  FExecutor := Utilities.CheckNotNullAndSet(StatementExecutor, 'StatementExecutor');
  FParams := TCollections.CreateDictionary<string, TParamDescriptor>([Spring.Collections.doOwnsValues]);
  FMethods := TCollections.CreateDictionary<string, TMethodDescriptor>([Spring.Collections.doOwnsValues]);

  ProcessAllAttributes;
  ValidateStatement;

  // obtain query definition to get SQL from resources
  FResourcedSQL := ExtractSQLString(ResReader.GetStringResource(SQLResource));
end;

procedure TVirtualQuery<TRecord, T>.DefineStatement(
  const Method: TRttiMethod;
  const Args: TArray<TValue>);
var
  ParamsList: IList<TParamDescriptor>;
begin
  ParamsList := TCollections.CreateList<TParamDescriptor>;

  // prepare our list of paramters prior to defining them in executor
  // so we know their directions and are able to define parameter list
  FParameterCommaList := '';

  TCollections.CreateList<TRttiParameter>(Method.GetParameters).ForEach(
    procedure(const Arg: TRttiParameter)
    var
      IsPagingLimit: Boolean;
      IsPagingOffset: Boolean;
      SqlInjectTag: string;
    begin
      IsPagingLimit := False;
      IsPagingOffset := False;

      TCollections.CreateList<TCustomAttribute>(Arg.GetAttributes).ForEach(
        procedure(const Attribute: TCustomAttribute)
        begin
          if Attribute is PagingLimitAttribute then
            IsPagingLimit := True
          else if Attribute is PagingOffsetAttribute then
            IsPagingOffset := True
          else if Attribute is SqlInjectAttribute then
            SqlInjectTag := (Attribute as SqlInjectAttribute).Tag;
        end);

      ParamsList.Add(AddOrUpdateDescriptor(Arg.Name, Arg.ParamType, ptInput, IsPagingLimit, IsPagingOffset, SqlInjectTag));
    end);

  // TODO param values could also be set with setters

  // remove first ', ' from parameter list used by function definition
  Delete(FParameterCommaList, 1, 2);

  // tell Executor to construct object
  Executor.BuildObject(stQuery, ReplaceSqlInject(GetSQLData, Args));

  // define parameters in executor once Direction and ParameterList is finally established
  ParamsList
    .Where(function(const Item: TParamDescriptor): Boolean
      begin
        Result := not(Item.IsPagingLimit or Item.IsPagingOffset);
      end)
    .ForEach(procedure(const Item: TParamDescriptor)
      begin
        Executor.AddParameter(Item.MappedName, Item.DataType.FieldType, Item.Direction);
      end);

  Executor.Prepare;
end;

procedure TVirtualQuery<TRecord, T>.DoInvoke(
  Method: TRttiMethod;
  const Args: TArray<TValue>;
  out Result: TValue);
var
  MethodDesc: TMethodDescriptor;
begin
  MethodDesc := FMethods.GetValueOrDefault(Method.Name);

  // all methods should be cached and processed by now
  Assert(Assigned(MethodDesc) and (MethodDesc.Category in [mcExecute, mcDatasetOper]));

  case MethodDesc.Category of
    mcExecute:
      Execute(Method, Args, Result);

    mcDatasetOper:
      begin
        TestDatasetOpen(Method.Name);

        case MethodDesc.Operation of
          dsNext:
            FDataset.Next;
          dsFirst:
            FDataset.First;
          dsClose:
            FDataset.Close;
          dsEOF:
            Result := FDataset.Eof;
        end;
      end;
  end;
end;

procedure TVirtualQuery<TRecord, T>.Execute(
  const Method: TRttiMethod;
  const Args: TArray<TValue>;
  out Result: TValue);
var
  PagingLimit: Integer;
  PagingOffset: Integer;
begin
  // define statement (assign SQL data, declare parameters) if necessary
  if not GetIsDefined then
    DefineStatement(Method, Args);

  PagingLimit := -1;
  PagingOffset := -1;

  FParams.Values.ForEach(procedure(const Descriptor: TParamDescriptor)
    begin
      if (Descriptor.Direction in [ptInput, ptInputOutput]) and
         not(Descriptor.IsPagingLimit or Descriptor.IsPagingOffset) then
        // convert value to variant (stripping Nullable to its base type if necessary)
        Executor.SetParameterValue(Descriptor.MappedName, Descriptor.DataType.GetAsVariant(Args[Descriptor.Index]))
      else if Descriptor.IsPagingLimit then
        PagingLimit := Args[Descriptor.Index].AsInteger
      else if Descriptor.IsPagingOffset then
        PagingOffset := Args[Descriptor.Index].AsInteger;
    end);

  if (PagingLimit <> 0) then
    Executor.SetPaging(PagingLimit, PagingOffset);

  FDataset := Executor.Open;
  case ExecMethod.ReturnType of
    rtNone:
      ; // procedure, no result
    rtEnum:
      SetEnumeratorValue(Result);
    else
      ; // TODO raise unimplemented;
  end;
end;

function TVirtualQuery<TRecord, T>.ExtractSQLString(const ResString: string): string;
const
  ControlBlockStart = '/*';
  ControlBlockEnd = '*/';
var
  PosDescEnd: integer;
begin
  // TODO extraction of SQL can be remove once all query resource have their
  // control blocks (with e.g. Destiption string) removed

  Result := ResString;

  if not SameText(ControlBlockStart, Copy(Result, 1, Length(ControlBlockStart))) then
    Exit;

  // split the whole text on the first end of comment
  PosDescEnd := Pos(ControlBlockEnd, Result);
  if PosDescEnd = -1 then
    raise EFidoException.CreateFmt('Malformed SQL control block: should end with "%s"', [ControlBlockEnd]);

  // remove description block from the SQL command
  Delete(Result, 1, PosDescEnd + 2);
  Result := Trim(Result);
end;

function TVirtualQuery<TRecord, T>.GetDescription: string;
begin
  Result := FDescription;
end;

class function TVirtualQuery<TRecord, T>.GetInstance(
  const Container: TContainer;
  const StatementExecutorServiceName: string): TVirtualQuery<TRecord, T>;
begin
  if StatementExecutorServiceName.IsEmpty() then
    Result := TVirtualQuery<TRecord, T>.Create(
      Container.Resolve<IStringResourceReader>,
      Container.Resolve<IStatementExecutor>)
  else
    result := TVirtualQuery<TRecord, T>.Create(
      Container.Resolve<IStringResourceReader>,
      Container.Resolve<IStatementExecutor>(StatementExecutorServiceName));
end;

function TVirtualQuery<TRecord, T>.GetIsDefined: boolean;
begin
  Result := Executor.IsBuilt;
end;

function TVirtualQuery<TRecord, T>.GetIsGetterName(const Name: string): boolean;
begin
  Result := Name.StartsWith(GetterPrefix, true);
end;

function TVirtualQuery<TRecord, T>.GetMappedName(const Name: string): string;
begin
  Result := Name.ToUpper;
end;

function TVirtualQuery<TRecord, T>.GetSQLData: string;
begin
  Result := FResourcedSQL;
end;

function TVirtualQuery<TRecord, T>.GetSQLResource: string;
begin
  Result := FSQLResource;
end;

procedure TVirtualQuery<TRecord, T>.ProcessAllAttributes;
var
  Context: TRttiContext;
  RttiType: TRttiType;
begin
  Context := TRttiContext.Create;

  RttiType := Context.GetType(TypeInfo(T));

  // process interface-level attributes
  TCollections.CreateList<TCustomAttribute>(RttiType.GetAttributes).ForEach(
    procedure(const Attribute: TCustomAttribute)
    begin
      ProcessAttribute(Attribute);
    end);

  // process all methods (and their attributes)
  TCollections.CreateList<TRttiMethod>(RttiType.GetMethods).ForEach(
    procedure(const Method: TRttiMethod)
    begin
      ProcessMethod(Method, FMethods);
    end);

  // if no [Execute] found and only one method assume it is the one (unless already assigned to column)
  if not Assigned(ExecMethod) and (FMethods.Count = 1) then
    if FMethods.First.Value.Category = mcNone then
      ExecMethod := FMethods.First.Value;

  // set remaining methods to rows affected or colgetters
  FMethods.Values
    .Where(function(const Value: TMethodDescriptor): Boolean
      begin
        Result := Value.Category = mcNone;
      end)
    .ForEach(procedure(const Value: TMethodDescriptor)
      begin
        Value.Category := mcColGetter;
      end);
end;

procedure TVirtualQuery<TRecord, T>.ProcessAttribute(
  const Attribute: TCustomAttribute;
  const Method: TRttiMethod;
  const MethDesc: TMethodDescriptor);
begin
  // process interface-level attributes (Statement, Description and Map )
  if not Assigned(Method) then
  begin
    // 1. Description
    if Attribute is DescriptionAttribute then
      FDescription := DescriptionAttribute(Attribute).Line
    // 2. SQLResource
    else if Attribute is SQLResourceAttribute then
    begin
      FSQLResource := SQLResourceAttribute(Attribute).Data;
    end;
    { TODO 3. Map atribute(s) (unlimited number)
    else if Attribute is MapAttribute then
      L.Add(MapAttribute(Attribute).Line)    }
  end

  // method attributes (Execute and Column)
  else
  begin
    // 1. Execute (optional if there is only one method in interface)
    if (Attribute is ExecuteAttribute) and not Assigned(ExecMethod) then
      ExecMethod := MethDesc
    // 2. Column (optional) - overrides automatic name with provided value
    else if (Attribute is ColumnAttribute) then
      MethDesc.MappedName := ColumnAttribute(Attribute).Line
  end;
end;

procedure TVirtualQuery<TRecord, T>.ProcessMethod(
  const Method: TRttiMethod;
  const Collection: IDictionary<string, TMethodDescriptor>);
var
  MethodDesc: TMethodDescriptor;
  O: TDatasetOperation;
  S: string;
begin
  if not FMethods.TryGetValue(Method.Name, MethodDesc) then
  begin
    Assert(Method.MethodKind in [mkFunction, mkProcedure]);

    MethodDesc := TMethodDescriptor.Create;
    Collection.Add(Method.Name, MethodDesc);

    MethodDesc.Category := mcNone;
    MethodDesc.OriginalName := Method.Name;
    MethodDesc.Converter := nil;

    if not (Method.MethodKind = mkFunction) then
      MethodDesc.ReturnType := rtNone
    else if ContainsText(Method.ReturnType.QualifiedName, 'Spring.Collections.IReadonlyList<') then
    begin
      MethodDesc.ReturnType := rtEnum;
      ExecMethod := MethodDesc;
      MethodDesc.Category := mcExecute;
    end
    else
    begin
      MethodDesc.Converter := DataTypeConverter.GetDescriptor(Method.ReturnType);
    end;

    MethodDesc.MappedName := GetMappedName(Method.Name);
  end;

  // auto describe based in attributes
  TCollections.CreateList<TCustomAttribute>(Method.GetAttributes).ForEach(
    procedure(const Attribute: TCustomAttribute)
    begin
      ProcessAttribute(Attribute, Method, MethodDesc);
    end);

  // assign rest to columns for openables
  for O := TDatasetOperation(1) to High(TDatasetOperation) do
    if SameText(Method.Name, DatasetOperationNames[O]) then
    begin
      Assert(MethodDesc.Category = mcNone);
      Assert(GetIsGetterName(DatasetOperationNames[O]) = MethodDesc.IsFunction);

      MethodDesc.Category := mcDatasetOper;
      MethodDesc.Operation := O;
      MethodDesc.MappedName := EmptyStr;

      Break;
    end;
end;

procedure TVirtualQuery<TRecord, T>.RaiseError(
  const Msg: string;
  const Args: array of const);
begin
  raise EFidoVirtualQueryError.CreateFmt(Msg, Args);
end;

function TVirtualQuery<TRecord, T>.ReplaceSqlInject(
  const Sql: string;
  const Args: TArray<TValue>): string;
var
  FixedSql: string;
begin
  FixedSql := Sql;

  FParams.Values.ForEach(
    procedure(const Descriptor: TParamDescriptor)
    begin
      if not Descriptor.SqlInjectTag.IsEmpty then
        FixedSql := FixedSql.Replace(Format('%%%s%%', [Descriptor.SqlInjectTag]), Descriptor.DataType.GetAsVariant(Args[Descriptor.Index]), [rfReplaceAll]);
    end);

  Result := FixedSql
end;

procedure TVirtualQuery<TRecord, T>.SetEnumeratorValue(out Result: TValue);
var
  LiveList: IReadonlyList<TRecord>;
begin
  TestDatasetOpen('SetEnumeratorValue');
  LiveList := TDataSetAsReadonlyList<TRecord>.Create(FDataset);

  FList := TCollections.CreateList<TRecord>(LiveList.ToArray).AsReadOnly();
  Result := TValue.From<IReadOnlyList<TRecord>>(FList.Target);
end;

procedure TVirtualQuery<TRecord, T>.SetExecMethod(const Value: TMethodDescriptor);
begin
  Assert(Assigned(Value) and not Assigned(ExecMethod));
  FExecMethod := Value;
  FExecMethod.Category := mcDatasetOper;
end;

procedure TVirtualQuery<TRecord, T>.TestDatasetOpen(const MethodToBeCalled: string);
const
  EDatasetClosed = '"%s" not allowed on closed dataset';
begin
  if not Assigned(FDataset) or not FDataset.Active then
    RaiseError(EDatasetClosed, [MethodToBeCalled]);
end;

procedure TVirtualQuery<TRecord, T>.ValidateStatement;
begin
  if not Assigned(ExecMethod) then
    RaiseError('"Execute" attibute or method must be defined', []);

  if FSQLResource.IsEmpty then
    RaiseError('"StatementData" cannot be empty', []);
end;

end.
