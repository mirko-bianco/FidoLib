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

unit Fido.JSON.Marshalling;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.SysUtils,
  System.DateUtils,
  System.Classes,
  System.JSON,
  System.Hash,
  System.Variants,
  System.Generics.Defaults,
  System.Generics.Collections,

  Spring.Collections.Lists,
  Spring,
  Spring.Collections,

  Fido.Exceptions,
  Fido.Json.Utilities,
  Fido.Json.Mapping,
  Fido.Db.TypeConverter,
  Fido.VirtualStatement.Attributes,
  Fido.VirtualInterface,
  Fido.Types,
  Fido.VirtualQuery.Attributes,
  Fido.VirtualDto.Abstract;

type
  TJSONDTOMethodDescriptor = class
    OriginalName: string;     // original name of method
    MappedName: string;       // mapped name of column or parameter of a method itself (not its pareameters)
    Converter: TDataTypeDescriptor;
    Category: TMethodCategory;
    IsInterface: Boolean;
    IsEnumeration: Boolean;
    TypeInfo: PTypeInfo;
    Value: TValue;

    function IsArray: Boolean;
  end;

  EJSONVirtualDto = class(EFidoException);

  TJSONVirtualDto = class(TVirtualInterface)
  strict private const
    GetterPrefix = 'GET';
  strict private
    FRecordMethods: IDictionary<string, TJSONDTOMethodDescriptor>;

    function GetIsGetterName(const Name: string): boolean;
    procedure CacheColumns(const JSONObject: TJSONObject);
  protected
    function GetMappedName(const Name: string): string;
    procedure DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
    procedure ProcessDtoAttributes(const PIID: PTypeInfo);
    procedure ProcessMethod(const Method: TRttiMethod);
  public
    constructor Create(const PIID: PTypeInfo; const JSONObject: TJSONObject); overload;
    constructor Create(const PIID: PTypeInfo; const Json: string); overload;
    destructor Destroy; override;

    procedure AfterConstruction; override;
  end;

  EJSONUnmarshaller = class(EFidoException);

  JSONUnmarshaller = class
  strict private
    class function CreateReadonlyListAsValue(const ElementTypeInfo: PTypeInfo;
      const ValuesArray: array of string): TValue; static;
    class function CreateList<T>(const ValuesArray: array of string): TValue; static;

    class function ToReadonlyListOfInterfaces(const JSONString: string; const TypeInfo: PTypeInfo; const ElementTypeInfo: PTypeInfo): TValue; static;
    class function ToReadonlyListOfPrimitives(const JSONString: string; const ElementTypeInfo: PTypeInfo): TValue; static;

    class function ToInterface(const JSONString: string; const TypeInfo: PTypeInfo): IInterface; overload; static;
    class function ToInterfaceAsValue(const JSONString: string; const TypeInfo: PTypeInfo): TValue; overload; static;

    class function ToObject(const JSONString: string; const TypeInfo: PTypeInfo): TValue; overload; static;

    class function ToReadonlyList(const JSONString: string; const TypeInfo: PTypeInfo): TValue; overload; static;

    class function ToPrimitive(const Value: string; const TypeInfo: PTypeInfo): TValue; static;
    class function ToEnumeration(const Value: string; const TypeInfo: PTypeInfo): TValue; static;
  public
    class function &To<T>(const JSONString: string): T; overload; static;
    class function &To(const JSONString: string; const TypeInfo: PTypeInfo): TValue; overload; static;


  end;

  EJSONMarshaller = class(EFidoException);

  JSONMarshaller = class
  strict private
    class function FromObject(const Value: TObject; const TypInfo: PTypeInfo): string; static;
    class function FromInterface(const Value: TValue; const TypInfo: PTypeInfo): string; static;
    class function FromPrimitive(const Value: TValue; const TypInfo: PTypeInfo): string; static;
    class function FromReadonlyList(const Value: TValue; const TypInfo: PTypeInfo): string; static;

    class function FromReadonlyListOfInterfaces(const Value: TValue; const TypInfo: PTypeInfo; const ElementTypeInfo: PTypeInfo): string; static;
    class function FromReadonlyListOfPrimitives(const Value: TValue; const TypInfo: PTypeInfo; const ElementTypeInfo: PTypeInfo): string; static;
  public
    class function From<T>(const Value: T): string; overload; static;
    class function From(const Value: TValue; const TypInfo: PTypeInfo): string; overload; static;
  end;

const
  ArrayInterfaceName = 'spring.collections.ireadonlylist<';
  NullableName = 'spring.nullable';
  StringName = 'system.string';
  Int64Name = 'system.int64';
  IntegerName = 'system.integer';
  DateTimeName = 'system.tdatetime';
  DoubleName = 'system.double';
  ExtendedName = 'system.extended';
  CurrencyName = 'system.currency';
  BooleanName = 'system.boolean';
  GuidName = 'system.tguid';
  SmallintName = 'system.smallint';

implementation

uses
  Fido.DesignPatterns.Adapter.JSonArrayAsReadonlyList;

{ TJSONVirtualDto }

procedure TJSONVirtualDto.AfterConstruction;
begin
  inherited;

end;

procedure TJSONVirtualDto.CacheColumns(const JSONObject: TJSONObject);
var
  D: TPair<string, TJSONDTOMethodDescriptor>;
  ObjectValue: TJsonValue;
  LJSONObject: Shared<TJSONObject>;
begin
  LJSONObject := TJSONObject.Create;
  with JSONObject.GetEnumerator do
  begin
    while MoveNext do
      LJSONObject.Value.AddPair(GetCurrent.JsonString.Value.ToUpper, TJSONObject.ParseJSONValue(GetCurrent.JsonValue.ToJSON));
    Free;
  end;

  for D in FRecordMethods do
  begin
    ObjectValue := LJSONObject.Value.GetValue(D.Value.MappedName);
    if Assigned(ObjectValue) then
    begin
      if D.Value.IsInterface then
        D.Value.Value := (ObjectValue as TJSONObject).ToString
      else if D.Value.IsArray then
        D.Value.Value := (ObjectValue as TJSONArray).ToString
      else if D.Value.IsEnumeration then
        D.Value.Value := ObjectValue.Value
      else
        D.Value.Value := JSONUnmarshaller.&To(ObjectValue.Value, D.Value.TypeInfo)
    end;
  end;
end;

constructor TJSONVirtualDto.Create(const PIID: PTypeInfo; const Json: string);
var
  JSONObject: TJSONObject;
begin
  inherited Create(PIID, DoInvoke);

  FRecordMethods := TCollections.CreateDictionary<string, TJSONDTOMethodDescriptor>([doOwnsValues]);
  ProcessDtoAttributes(PIID);
  JSONObject := TJSonObject.ParseJSONValue(Json) as TJSONObject;
  try
    CacheColumns(JSONObject);
  finally
    JSONObject.Free;
  end;
end;

constructor TJSONVirtualDto.Create(const PIID: PTypeInfo; const JSONObject: TJSONObject);
begin
  inherited Create(PIID, DoInvoke);

  Guard.CheckNotNull(JSONObject, 'JSONObject');

  FRecordMethods := TCollections.CreateDictionary<string, TJSONDTOMethodDescriptor>([doOwnsValues]);

  ProcessDtoAttributes(PIID);
  CacheColumns(JSONObject);
end;

destructor TJSONVirtualDto.Destroy;
begin

  inherited;
end;

procedure TJSONVirtualDto.DoInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  MethodDesc: TJSONDTOMethodDescriptor;
begin
  inherited;

  MethodDesc := FRecordMethods.GetValueOrDefault(Method.Name);

  // all methods should be cached and processed by now
  Assert(Assigned(MethodDesc) and (MethodDesc.Category in [mcColGetter]));

  if MethodDesc.IsInterface or MethodDesc.IsEnumeration or MethodDesc.IsArray then
    Result := JSONUnmarshaller.&To(MethodDesc.Value.AsString, Method.ReturnTypeHandle)
  else
    Result := MethodDesc.Value;
end;

function TJSONVirtualDto.GetIsGetterName(const Name: string): boolean;
begin
  Result := Name.StartsWith(GetterPrefix, true);
end;

function TJSONVirtualDto.GetMappedName(const Name: string): string;
begin
  // TODO use actual Maps
  Result := Name.ToUpper;

  // remove getter prefix; TODO setters?
  if GetIsGetterName(Result) then
    Delete(Result, 1, Length(GetterPrefix));
end;

procedure TJSONVirtualDto.ProcessDtoAttributes(const PIID: PTypeInfo);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
  Pair: TPair<string, TJSONDTOMethodDescriptor>;
begin
  inherited;

  Context := TRttiContext.Create;

  RttiType := Context.GetType(PIID);

  // process all methods (and their attributes)
  for Method in RttiType.GetMethods do
    ProcessMethod(Method);

  // set remaining methods to rows affected or colgetters
  for Pair in FRecordMethods do
    with Pair.Value do
      if Category = mcNone then
        Category := mcColGetter;
end;

procedure TJSONVirtualDto.ProcessMethod(const Method: TRttiMethod);
var
  Attribute : TCustomAttribute;
  MethodDesc: TJSONDTOMethodDescriptor;
begin
  inherited;
  if not FRecordMethods.TryGetValue(Method.Name, MethodDesc) then
  begin
    if not (Method.MethodKind in [mkFunction]) then
      Exit;

    MethodDesc := TJSONDTOMethodDescriptor.Create;
    FRecordMethods.Add(Method.Name, MethodDesc);

    MethodDesc.Category := mcNone;
    MethodDesc.OriginalName := Method.Name;
    MethodDesc.Converter := nil;
    MethodDesc.IsInterface := False;
    MethodDesc.IsEnumeration := False;
    MethodDesc.TypeInfo := Method.ReturnType.Handle;

    if not Method.ReturnType.QualifiedName.ToLower.StartsWith(ArrayInterfaceName) then
      if Method.ReturnType.TypeKind = tkInterface then
        MethodDesc.IsInterface := True
      else if (Method.ReturnType.TypeKind = tkEnumeration) and
              (not Method.ReturnType.QualifiedName.ToLower.Equals(BooleanName)) then
        MethodDesc.IsEnumeration := True
      else
        MethodDesc.Converter := DataTypeConverter.GetDescriptor(Method.ReturnType);

    MethodDesc.MappedName := GetMappedName(Method.Name);
  end;

  // auto describe based in attributes
  for Attribute in Method.GetAttributes do
    if (Attribute is ColumnAttribute) then
      MethodDesc.MappedName := ColumnAttribute(Attribute).Line;
end;

{ JSONVirtualDto }

class function JSONUnmarshaller.ToReadonlyList(const JSONString: string;
  const TypeInfo: PTypeInfo): TValue;
var
  Context: TRttiContext;
  ListRttiType: TRttiType;
  ElementRttiType: TRttiType;
  TypeName: string;
begin
  Context := TRttiContext.Create;

  ListRttiType := Context.GetType(TypeInfo);

  TypeName := Copy(
    ListRttiType.QualifiedName,
    Pos('<', ListRttiType.QualifiedName) + 1,
    Length(ListRttiType.QualifiedName));
  TypeName := Copy(TypeName, 1, Pos('>', TypeName) - 1);

  ElementRttiType := Context.FindType(TypeName);
  if ElementRttiType.TypeKind = tkInterface then
    Result := JSONUnmarshaller.ToReadonlyListOfInterfaces(JSONString, TypeInfo, ElementRttiType.Handle)
  else
    Result := JSONUnmarshaller.ToReadonlyListOfPrimitives(JSONString, ElementRttiType.Handle)
end;

class function JSONUnmarshaller.ToReadonlyListOfInterfaces(
  const JSONString: string;
  const TypeInfo: PTypeInfo;
  const ElementTypeInfo: PTypeInfo): TValue;
var
  JsonArray: Shared<TJSONArray>;
  IntfList: IReadOnlyList<IInterface>;
begin
  JsonArray := TJSONObject.ParseJSONValue(JSONString) as TJSONArray;

  IntfList := TJsonArrayAsReadonlyInterfaceList<IInterface>.Create(JSONArray.Value, ElementTypeInfo);
  TValue.Make(@IntfList, TypeInfo, Result);
end;

class function JSONUnmarshaller.ToReadonlyListOfPrimitives(
  const JSONString: string;
  const ElementTypeInfo: PTypeInfo): TValue;
var
  JsonArray: Shared<TJSONArray>;
  ValuesArray: TArray<string>;
  Index: Integer;
begin
  JsonArray := TJSONObject.ParseJSONValue(JSONString) as TJSONArray;

  SetLength(ValuesArray, JsonArray.Value.Count);
  for Index := 0 to JsonArray.Value.Count - 1 do
    ValuesArray[Index] := JsonArray.Value.Items[Index].GetValue<string>;

  Result := CreateReadonlyListAsValue(ElementTypeInfo, ValuesArray);
end;

class function JSONUnmarshaller.ToInterfaceAsValue(const JSONString: string; const TypeInfo: PTypeInfo): TValue;
var
  Intf: IInterface;
begin
  Intf := JSONUnmarshaller.ToInterface(JSONString, TypeInfo);
  TValue.Make(@Intf, TypeInfo, Result);
end;

class function JSONUnmarshaller.&To<T>(const JSONString: string): T;
var
  Context: TRttiContext;
  RttiType: TRttiType;
begin
  Context := TRttiContext.Create;
  RttiType := Context.GetType(TypeInfo(T));

  Result := JSONUnmarshaller.To(JSONString, RttiType.Handle).AsType<T>;
end;

class function JSONUnmarshaller.ToEnumeration(const Value: string;
  const TypeInfo: PTypeInfo): TValue;
begin
  Result := MappingsUtilities.GetEnumeratives.&To(Value, TypeInfo);
end;

class function JSONUnmarshaller.ToPrimitive(
  const Value: string;
  const TypeInfo: PTypeInfo): TValue;
var
  Mapping: TJSONMarshallingMapping;
begin
  if MappingsUtilities.TryGetPrimitiveType(TypeInfo, Mapping) then
    Result := Mapping.&To(Value, TypeInfo);
end;

class function JSONUnmarshaller.&To(
  const JSONString: string;
  const TypeInfo: PTypeInfo): TValue;
var
  Context: TRttiContext;
  RttiType: TRttiType;
begin
  Context := TRttiContext.Create;
  RttiType := Context.GetType(TypeInfo);

  case RttiType.TypeKind of
    tkUnknown,
    tkSet,
    tkMethod,
    tkClassRef,
    tkPointer,
    tkProcedure,
    tkArray,
    tkDynArray,
    tkVariant: raise EJSONUnmarshaller.CreateFmt('JSONUnmarshaller.To<T> does not support type "%s"', [RttiType.QualifiedName]);

    tkEnumeration: begin
      if RttiType.QualifiedName.ToLower.Equals(BooleanName) then
        Result := JSONUnmarshaller.ToPrimitive(JSONString, RttiType.Handle)
      else
        Result := JSONUnmarshaller.ToEnumeration(JSONString, RttiType.Handle);
    end;
    tkClass: Result := JSONUnmarshaller.ToObject(JSONString, TypeInfo);
    tkInterface: begin
      if RttiType.QualifiedName.ToLower.StartsWith(ArrayInterfaceName) then
        Result := JSONUnmarshaller.ToReadonlyList(JSONString, TypeInfo)
      else
        Result := JSONUnmarshaller.ToInterfaceAsValue(JSONString, TypeInfo);
    end;

    tkChar,
    tkString,
    tkWChar,
    tkLString,
    tkWString,
    tkUString,
    tkFloat,
    tkInteger,
    tkInt64: Result := JSONUnmarshaller.ToPrimitive(JSONString, RttiType.Handle);

    tkRecord: begin
      if RttiType.QualifiedName.ToLower.StartsWith(NullableName) or
         RttiType.QualifiedName.ToLower.Equals(GuidName)then
        Result := JSONUnmarshaller.ToPrimitive(JSONString, RttiType.Handle)
      else
        raise EJSONUnmarshaller.CreateFmt('JSONUnmarshaller.To<T> does not support type "%s"', [RttiType.QualifiedName]);
    end;
  end;
end;

class function JSONUnmarshaller.CreateList<T>(const ValuesArray: array of string): TValue;
var
  List: IList<T>;
  ConvertedArray: TArray<T>;
  Index: Integer;
begin
  SetLength(ConvertedArray, Length(ValuesArray));

  for Index := 0 to Length(ValuesArray) - 1 do
    ConvertedArray[Index] := JSONUnmarshaller.&To<T>(ValuesArray[Index]);

  List := TCollections.CreateList<T>(ConvertedArray);
  Result := TValue.From<IReadOnlyList<T>>(List as IReadOnlyList<T>);
end;

class function JSONUnmarshaller.CreateReadonlyListAsValue(
  const ElementTypeInfo: PTypeInfo;
  const ValuesArray: array of string): TValue;
var
  Context: TRttiContext;
  RttiType: TRttiType;
begin
  Context := TRttiContext.Create;

  RttiType := Context.GetType(ElementTypeInfo);

  if RttiType.QualifiedName.ToLower = StringName then
    Result := CreateList<string>(ValuesArray)
  else if RttiType.QualifiedName.ToLower = IntegerName then
    Result := CreateList<Integer>(ValuesArray)
  else if RttiType.QualifiedName.ToLower = Int64Name then
    Result := CreateList<Int64>(ValuesArray)
  else if RttiType.QualifiedName.ToLower = GuidName then
    Result := CreateList<TGuid>(ValuesArray)
  else if RttiType.QualifiedName.ToLower = BooleanName then
    Result := CreateList<Boolean>(ValuesArray)
  else if RttiType.QualifiedName.ToLower = DatetimeName then
    Result := CreateList<TDateTime>(ValuesArray)
  else if RttiType.QualifiedName.ToLower = DoubleName then
    Result := CreateList<Double>(ValuesArray)
  else if RttiType.QualifiedName.ToLower = CurrencyName then
    Result := CreateList<Currency>(ValuesArray)
  else if RttiType.QualifiedName.ToLower = ExtendedName then
    Result := CreateList<Extended>(ValuesArray)
  else
    raise EJSONVirtualDto.CreateFmt('%s is not a supported type for TJSONVirtualDto arrays.', [RttiType.QualifiedName]);
end;

class function JSONUnmarshaller.ToInterface(const JSONString: string; const TypeInfo: PTypeInfo): IInterface;
var
  RInterface: IInterface;
  JSONObject: Shared<TJSONObject>;
begin
  JSONObject := TJSONObject.ParseJSONValue(JSONString) as TJSONObject;
  Supports(TJSONVirtualDto.Create(TypeInfo, JSONObject.Value), GetTypeData(TypeInfo)^.Guid, RInterface);
  Result := RInterface;
end;

class function JSONUnmarshaller.ToObject(
  const JSONString: string;
  const TypeInfo: PTypeInfo): TValue;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  InstanceType: TRttiInstanceType;
  Prop: TRttiProperty;
  Method: TRttiMethod;
  Param: TValue;
  JSONObject: Shared<TJSONObject>;
begin
  JSONObject := TJSONObject.ParseJSONValue(JSONString) as TJSONObject;
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(TypeInfo);
  InstanceType := RttiType.AsInstance;
  Result := InstanceType.GetMethod('Create').Invoke(InstanceType.MetaclassType, []).Convert(TypeInfo);
  with JSONObject.Value.GetEnumerator do
  begin
    while MoveNext do
    begin
      Prop := RttiType.GetProperty(GetCurrent.JsonString.Value);
      if Assigned(Prop) and Prop.IsWritable then
        Prop.SetValue(Result.AsObject, JSONUnmarshaller.To(GetCurrent.JsonValue.Value, Prop.PropertyType.Handle))
      else
      begin
        Method := RttiType.GetMethod(Format('Set%s', [GetCurrent.JsonString.Value]));
        if Assigned(Method) and (Method.MethodKind = mkProcedure) and (Length(Method.GetParameters) = 1) then
        begin
          Param := JSONUnmarshaller.To(GetCurrent.JsonValue.Value, Prop.PropertyType.Handle);
          Method.Invoke(Result.AsObject, [Param]);
        end;
      end;
    end;
    Free;
  end;
end;

{ JSONMarshaller }

class function JSONMarshaller.From(const Value: TValue;
  const TypInfo: PTypeInfo): string;
var
  Context: TRttiContext;
  RttiType: TRttiType;
begin
  Context := TRttiContext.Create;
  RttiType := Context.GetType(TypInfo);

  case RttiType.TypeKind of
    tkUnknown,
    tkSet,
    tkMethod,
    tkClassRef,
    tkPointer,
    tkProcedure,
    tkArray,
    tkDynArray,
    tkVariant: raise EJSONMarshaller.CreateFmt('JSONMarshaller.From<T> does not support type "%s"', [RttiType.QualifiedName]);

    tkEnumeration: begin
      if RttiType.QualifiedName.ToLower.Equals(BooleanName) then
        Result := JSONMarshaller.FromPrimitive(Value, TypInfo)
      else
        Result := MappingsUtilities.GetEnumeratives.From(Value);
    end;
    tkClass: Result := JSONMarshaller.FromObject(Value.AsObject, TypInfo);
    tkInterface: begin
      if RttiType.QualifiedName.ToLower.StartsWith(ArrayInterfaceName) then
        Result := JSONMarshaller.FromReadonlyList(Value, TypInfo)
      else
        Result := JSONMarshaller.FromInterface(Value, TypInfo);
    end;

    tkChar,
    tkString,
    tkWChar,
    tkLString,
    tkWString,
    tkUString,
    tkFloat,
    tkInteger,
    tkInt64: Result := JSONMarshaller.FromPrimitive(Value, RttiType.Handle);

    tkRecord: begin
      if RttiType.QualifiedName.ToLower.StartsWith(NullableName) or
         RttiType.QualifiedName.ToLower.StartsWith(GuidName) then
        Result := JSONMarshaller.FromPrimitive(Value, RttiType.Handle)
      else
        raise EJSONMarshaller.CreateFmt('JSONMarshaller.From<T> does not support type "%s"', [RttiType.QualifiedName]);
    end;
  end;
end;

class function JSONMarshaller.From<T>(const Value: T): string;
begin
  Result := JSONMarshaller.From(TValue.From<T>(Value), TypeInfo(T));
end;

class function JSONMarshaller.FromInterface(const Value: TValue; const TypInfo: PTypeInfo): string;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
  Prop: TRttiProperty;
  ReturnValue: TValue;
  JSONObject: Shared<TJSONObject>;
begin
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(TypInfo);

  JSONObject := TJSONObject.Create;

  for Method in RttiType.GetMethods do
  begin
    if (Method.Visibility in [mvPublic, mvPublished]) and
       (Method.MethodKind = mkFunction) and
       (Length(Method.GetParameters) = 0) then
    begin
      ReturnValue := Method.Invoke(Value, []);
      try
        JSONObject.Value.AddPair(Method.Name, JSONMarshaller.From(ReturnValue, Method.ReturnType.Handle));
      except
      end;
    end;
  end;

  for Prop in RttiType.GetProperties do
  begin
    if (Prop.Visibility in [mvPublic, mvPublished]) and
       Prop.IsReadable then
    begin
      ReturnValue := Prop.GetValue(Value.AsInterface);
      try
        JSONObject.Value.AddPair(Prop.Name, JSONMarshaller.From(ReturnValue, Prop.PropertyType.Handle));
      except
      end;
    end;
  end;

  Result := JSONObject.Value.ToJson;
end;

class function JSONMarshaller.FromObject(const Value: TObject; const TypInfo: PTypeInfo): string;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
  Prop: TRttiProperty;
  ReturnValue: TValue;
  JSONObject: Shared<TJSONObject>;
begin
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(TypInfo);

  JSONObject := TJSONObject.Create;

  for Method in RttiType.GetMethods do
  begin
    if (Method.Visibility in [mvPublished]) and
       (Method.MethodKind = mkFunction) and
       (Length(Method.GetParameters) = 0) then
    begin
      ReturnValue := Method.Invoke(Value, []);
      try
        JSONObject.Value.AddPair(Method.Name, JSONMarshaller.From(ReturnValue, Method.ReturnType.Handle));
      except
      end;
    end;
  end;

  for Prop in RttiType.GetProperties do
  begin
    if (Prop.Visibility in [mvPublished]) and
       Prop.IsReadable then
    begin
      ReturnValue := Prop.GetValue(Value);
      try
        JSONObject.Value.AddPair(Prop.Name, JSONMarshaller.From(ReturnValue, Prop.PropertyType.Handle));
      except
      end;
    end;
  end;

  Result := JSONObject.Value.ToJson;
end;

class function JSONMarshaller.FromPrimitive(const Value: TValue; const TypInfo: PTypeInfo): string;
var
  Mapping: TJSONMarshallingMapping;
begin
  if MappingsUtilities.TryGetPrimitiveType(TypInfo, Mapping) then
    Result := Mapping.From(Value);
end;

class function JSONMarshaller.FromReadonlyList(const Value: TValue;
  const TypInfo: PTypeInfo): string;
var
  Context: TRttiContext;
  ListRttiType: TRttiType;
  ElementRttiType: TRttiType;
  TypeName: string;
begin
  Context := TRttiContext.Create;

  ListRttiType := Context.GetType(TypInfo);

  TypeName := Copy(
    ListRttiType.QualifiedName,
    Pos('<', ListRttiType.QualifiedName) + 1,
    Length(ListRttiType.QualifiedName));
  TypeName := Copy(TypeName, 1, Pos('>', TypeName) - 1);

  ElementRttiType := Context.FindType(TypeName);
  if ElementRttiType.TypeKind = tkInterface then
    Result := JSONMarshaller.FromReadonlyListOfInterfaces(Value, TypInfo, ElementRttiType.Handle)
  else
    Result := JSONMarshaller.FromReadonlyListOfPrimitives(Value, TypInfo, ElementRttiType.Handle)
end;

class function JSONMarshaller.FromReadonlyListOfInterfaces(
  const Value: TValue;
  const TypInfo, ElementTypeInfo: PTypeInfo): string;
var
  JsonArray: Shared<TJSONArray>;
  Intf: IInterface;
  Item: TValue;
begin
  JsonArray := TJSONArray.Create;

  for Intf in Value.Convert(TypeInfo(IReadOnlyList<IInterface>)).AsType<IReadOnlyList<IInterface>> do
    if Supports(Intf, GetTypeData(ElementTypeInfo).GUID, Item) then
      JsonArray.Value.AddElement(TJSONObject.ParseJSONValue(JSONMarshaller.FromInterface(Item, ElementTypeInfo)));

  Result := JsonArray.Value.ToJSON;
end;

class function JSONMarshaller.FromReadonlyListOfPrimitives(
  const Value: TValue;
  const TypInfo: PTypeInfo;
  const ElementTypeInfo: PTypeInfo): string;
var
  JsonArray: Shared<TJSONArray>;
  Intf: IReadonlyList;
begin
  JsonArray := TJSONArray.Create;

  if Supports(Value.Convert(TypInfo).AsInterface, GetTypeData(TypInfo).GUID, Intf) then
    with Intf.GetEnumerator do
      while MoveNext do
        JsonArray.Value.AddElement(TJSONObject.ParseJSONValue(JSONMarshaller.FromPrimitive(Current, ElementTypeInfo)));

  Result := JsonArray.Value.ToJSON;
end;

{ TJSONDTOMethodDescriptor }

function TJSONDTOMethodDescriptor.IsArray: Boolean;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
begin
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(TypeInfo);
  Result := RttiType.QualifiedName.ToLower.StartsWith(ArrayInterfaceName);
end;

end.
