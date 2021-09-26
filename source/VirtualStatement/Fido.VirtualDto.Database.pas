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

unit Fido.VirtualDto.Database;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.Classes,
  Data.DB,

  Spring.Collections,

  Fido.Db.TypeConverter,
  Fido.VirtualStatement.Attributes,
  Fido.VirtualInterface,
  Fido.Types,
  Fido.VirtualQuery.Attributes,
  Fido.VirtualDto.Abstract;

type
  TDatabaseVirtualDto<T: IInterface> = class(TAbstractVirtualDto<T>)
  strict private
    FDataset: TDataSet;
    FRecordMethods: IDictionary<string, TMethodDescriptor>;

    procedure CacheColumns(const Dataset: TDataSet);
  protected
    procedure DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue); override;
    procedure ProcessDtoAttributes; override;
    procedure ProcessMethod(const Method: TRttiMethod); override;
  public
    constructor Create(const Dataset: TDataSet);

    procedure AfterConstruction; override;
  end;

  DatabaseVirtualDto<T: IInterface> = class
    class function New(const Dataset: TDataSet): T; static;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,

  Spring;

{ TDatabaseVirtualDto<T> }

procedure TDatabaseVirtualDto<T>.AfterConstruction;
begin
  inherited;
  CacheColumns(FDataset);
end;

procedure TDatabaseVirtualDto<T>.CacheColumns(const Dataset: TDataSet);
var
  D: TPair<string, TMethodDescriptor>;
  TableField: TField;
begin
  for D in FRecordMethods do
  begin
    TableField := Dataset.FindField(D.Value.MappedName);
    if Assigned(TableField) then
      D.Value.FieldValue := TableField.Value;
  end;
end;

constructor TDatabaseVirtualDto<T>.Create(const Dataset: TDataSet);
begin
  inherited Create;
  Guard.CheckNotNull(Dataset, 'Dataset');
  FDataset := Dataset;

  FRecordMethods := TCollections.CreateDictionary<string, TMethodDescriptor>([doOwnsValues]);
end;

procedure TDatabaseVirtualDto<T>.DoInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  MethodDesc: TMethodDescriptor;
begin
  inherited;

  MethodDesc := FRecordMethods.GetValueOrDefault(Method.Name);

  // all methods should be cached and processed by now
  Assert(Assigned(MethodDesc) and (MethodDesc.Category in [mcColGetter]));
  Result := MethodDesc.Converter.GetFromVariant(MethodDesc.FieldValue);
end;

procedure TDatabaseVirtualDto<T>.ProcessDtoAttributes;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Attribute: TCustomAttribute;
  Method: TRttiMethod;
  Pair: TPair<string, TMethodDescriptor>;
begin
  inherited;

  Context := TRttiContext.Create;

  RttiType := Context.GetType(TypeInfo(T));

  // process all methods (and their attributes)
  for Method in RttiType.GetMethods do
    ProcessMethod(Method);

  // set remaining methods to rows affected or colgetters
  for Pair in FRecordMethods do
    with Pair.Value do
      if Category = mcNone then
        Category := mcColGetter;
end;

procedure TDatabaseVirtualDto<T>.ProcessMethod(const Method: TRttiMethod);
var
  Attribute : TCustomAttribute;
  MethodDesc: TMethodDescriptor;
  O: TDatasetOperation;
  S: string;
begin
  inherited;
  if not FRecordMethods.TryGetValue(Method.Name, MethodDesc) then
  begin
    Assert(Method.MethodKind in [mkFunction]);

    MethodDesc := TMethodDescriptor.Create;
    FRecordMethods.Add(Method.Name, MethodDesc);

    MethodDesc.Category := mcNone;
    MethodDesc.OriginalName := Method.Name;
    MethodDesc.Converter := nil;

    MethodDesc.Converter := DataTypeConverter.GetDescriptor(Method.ReturnType);

    MethodDesc.MappedName := GetMappedName(Method.Name);
  end;

  // auto describe based in attributes
  for Attribute in Method.GetAttributes do
    if (Attribute is ColumnAttribute) then
      MethodDesc.MappedName := ColumnAttribute(Attribute).Line;
end;

{ DatabaseVirtualDto<T> }

class function DatabaseVirtualDto<T>.New(const Dataset: TDataSet): T;
var
  RInterface: T;
begin
  Supports(TDatabaseVirtualDto<T>.Create(DataSet), GetTypeData(TypeInfo(T))^.Guid, RInterface);
  Result := RInterface;
end;

end.
