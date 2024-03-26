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

  Fido.DesignPatterns.Observer.Intf,
  Fido.DesignPatterns.Observer.Notification.Intf,
  Fido.DesignPatterns.Observer.Notification,
  Fido.DesignPatterns.Observable.Intf,
  Fido.DesignPatterns.Observable.Delegated,
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
  EJSONVirtualDto = class(EFidoException);

  TJSONVirtualDto = class(TVirtualInterface)
  strict private type
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
  strict private const
    GetterPrefix = 'GET';
  strict private
    FObservable: IObservable;
    FRecordMethods: IDictionary<string, TJSONDTOMethodDescriptor>;

    function GetIsGetterName(const Name: string): boolean;
    procedure CacheColumns(const JSONObject: TJSONObject; const ConfigurationName: string);
  protected
    function GetMappedName(const Name: string): string;
    procedure DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
    procedure ProcessDtoAttributes(const PIID: PTypeInfo);
    procedure ProcessMethod(const Method: TRttiMethod);
  public
    constructor Create(const PIID: PTypeInfo; const JSONObject: TJSONObject; const ConfigurationName: string = ''); overload;
    constructor Create(const PIID: PTypeInfo; const Json: string; const ConfigurationName: string = ''); overload;
    destructor Destroy; override;

    procedure RegisterObserver(const Observer: IObserver);
    procedure UnregisterObserver(const Observer: IObserver);
    function GetIdentity: string;
    procedure Broadcast(const Notification: INotification); overload;
    procedure Broadcast(const Description: string); overload;
    procedure Broadcast(const Description: string; const Data: TNotificationData); overload;
    function IsPaused: boolean;
    procedure Pause;
    procedure Resume(const AndBroadcast: string = '');
  end;

  EJSONUnmarshaller = class(EFidoException);

  JSONUnmarshaller = class
  strict private
    class function CreateReadonlyListAsValue(const ElementTypeInfo: PTypeInfo; const ValuesArray: array of string; const ConfigurationName: string): TValue; static;
    class function CreateList<T>(const ValuesArray: array of string; const ConfigurationName: string): TValue; static;
    class function ToReadonlyListOfInterfaces(const JSONString: string; const TypeInfo: PTypeInfo; const ElementTypeInfo: PTypeInfo): TValue; static;
    class function ToReadonlyListOfObjects(const JSONString: string; const TypeInfo: PTypeInfo; const ElementTypeInfo: PTypeInfo): TValue; static;
    class function ToReadonlyListOfPrimitives(const JSONString: string; const ElementTypeInfo: PTypeInfo; const ConfigurationName: string): TValue; static;
    class function ToReadonlyListOfEnums(const JSONString: string; const TypInfo: PTypeInfo; const ConfigurationName: string): TValue; static;
    class function ToInterface(const JSONString: string; const TypeInfo: PTypeInfo; const ConfigurationName: string): IInterface; overload; static;
    class function ToInterfaceAsValue(const JSONString: string; const TypeInfo: PTypeInfo; const ConfigurationName: string): TValue; overload; static;
    class function ToObject(const JSONString: string; const TypeInfo: PTypeInfo; const ConfigurationName: string): TValue; overload; static;
    class function ToRecord(const JSONString: string; const TypeInfo: PTypeInfo; const ConfigurationName: string): TValue; overload; static;
    class function ToReadonlyList(const JSONString: string; const TypeInfo: PTypeInfo; const ConfigurationName: string): TValue; overload; static;
    class function ToPrimitive(const Value: string; const TypeInfo: PTypeInfo; const ConfigurationName: string): TValue; static;
    class function ToEnumeration(const Value: string; const TypeInfo: PTypeInfo; const ConfigurationName: string): TValue; static;
  public
    class function &To<T>(const JSONString: string; const ConfigurationName: string = ''): T; overload; static;
    class function &To(const JSONString: string; const TypeInfo: PTypeInfo; const ConfigurationName: string = ''): TValue; overload; static;
  end;

  EJSONMarshaller = class(EFidoException);

  JSONMarshaller = class
  strict private
    class function FromRecord(const Value: TValue; const TypInfo: PTypeInfo; const ConfigurationName: string): Nullable<string>; static;
    class function FromObject(const Value: TObject; const TypInfo: PTypeInfo; const ConfigurationName: string): Nullable<string>; static;
    class function FromInterface(const Value: TValue; const TypInfo: PTypeInfo; const ConfigurationName: string): Nullable<string>; static;
    class function FromPrimitive(const Value: TValue; const TypInfo: PTypeInfo; const ConfigurationName: string): Nullable<string>; static;
    class function FromEnumeration(const Value: TValue; const ConfigurationName: string): Nullable<string>; static;
    class function FromReadonlyList(const Value: TValue; const TypInfo: PTypeInfo; const ConfigurationName: string): string; static;
    class function FromReadonlyListOfInterfaces(const Value: TValue; const TypInfo: PTypeInfo; const ElementTypeInfo: PTypeInfo; const ConfigurationName: string): string; static;
    class function FromReadonlyListOfObjects(const Value: TValue; const TypInfo: PTypeInfo; const ElementTypeInfo: PTypeInfo; const ConfigurationName: string): string; static;
    class function FromReadonlyListOfPrimitives(const Value: TValue; const TypInfo: PTypeInfo; const ElementTypeInfo: PTypeInfo; const ConfigurationName: string): string; static;
    class function FromReadonlyListOfEnums(const Value: TValue; const TypInfo: PTypeInfo; const ConfigurationName: string): string; static;
    class function FromReadonlyListOfRecords(const Value: TValue; const TypInfo: PTypeInfo; const ElementTypeInfo: PTypeInfo; const ConfigurationName: string): string; static;
    class function InternalFrom(const Value: TValue; const TypInfo: PTypeInfo; const ConfigurationName: string = ''): TJsonValue; static;
  public
    class function From<T>(const Value: T; const ConfigurationName: string = ''): string; overload; static;
    class function From(const Value: TValue; const TypInfo: PTypeInfo; const ConfigurationName: string = ''): string; overload; static;
  end;

const
  ArrayInterfaceName = 'spring.collections.ireadonlylist<';
  NullableName = 'nullable';
  StringName = 'system.string';
  Int64Name = 'system.int64';
  IntegerName = 'system.integer';
  DateTimeName = 'system.tdatetime';
  DoubleName = 'system.double';
  ExtendedName = 'system.extended';
  CurrencyName = 'system.currency';
  BooleanName = 'system.boolean';
  GuidName = 'tguid';
  SmallintName = 'system.smallint';

implementation

uses
  Fido.DesignPatterns.Adapter.JSonArrayAsReadonlyList;

{ TJSONVirtualDto }

procedure TJSONVirtualDto.Broadcast(const Notification: INotification);
begin
  FObservable.Broadcast(Notification);
end;

procedure TJSONVirtualDto.Broadcast(const Description: string);
begin
  FObservable.Broadcast(Description);
end;

procedure TJSONVirtualDto.Broadcast(const Description: string;
  const Data: TNotificationData);
begin
  FObservable.Broadcast(Description, Data);
end;

procedure TJSONVirtualDto.CacheColumns(
  const JSONObject: TJSONObject;
  const ConfigurationName: string);
var
  LJSONObject: IShared<TJSONObject>;
begin
  LJSONObject := Shared.Make(TJSONObject.Create);
  with JSONObject.GetEnumerator do
  begin
    while MoveNext do
      LJSONObject.AddPair(GetCurrent.JsonString.Value.ToUpper, TJSONObject.ParseJSONValue(GetCurrent.JsonValue.ToJSON));
    Free;
  end;

  FRecordMethods.Values.ForEach(
    procedure(const Value: TJSONDTOMethodDescriptor)
    var
      ObjectValue: TJsonValue;
    begin
      ObjectValue := LJSONObject.GetValue(Value.MappedName);
      if not Assigned(ObjectValue) then
        Exit;

      if Value.IsInterface then
        Value.Value := (ObjectValue as TJSONObject).ToString
      else if Value.IsArray then
      begin
        if ObjectValue.Value <> '[]' then
          Value.Value := (ObjectValue as TJSONArray).ToString;
      end
      else if Value.IsEnumeration then
        Value.Value := ObjectValue.Value
      else
        Value.Value := JSONUnmarshaller.&To(ObjectValue.Value, Value.TypeInfo, ConfigurationName)
    end);
end;

constructor TJSONVirtualDto.Create(
  const PIID: PTypeInfo;
  const Json: string;
  const ConfigurationName: string);
var
  JSONObject: IShared<TJSONObject>;
begin
  inherited Create(PIID, DoInvoke);

  FRecordMethods := TCollections.CreateDictionary<string, TJSONDTOMethodDescriptor>([doOwnsValues]);
  ProcessDtoAttributes(PIID);
  JSONObject := Shared.Make(TJSonObject.ParseJSONValue(Json) as TJSONObject);
  CacheColumns(JSONObject, ConfigurationName);

  FObservable := TDelegatedObservable.Create(nil);
end;

constructor TJSONVirtualDto.Create(
  const PIID: PTypeInfo;
  const JSONObject: TJSONObject;
  const ConfigurationName: string);
begin
  inherited Create(PIID, DoInvoke);

  Guard.CheckNotNull(JSONObject, 'JSONObject');

  FRecordMethods := TCollections.CreateDictionary<string, TJSONDTOMethodDescriptor>([doOwnsValues]);

  ProcessDtoAttributes(PIID);
  CacheColumns(JSONObject, ConfigurationName);
  FObservable := TDelegatedObservable.Create(nil);
end;

destructor TJSONVirtualDto.Destroy;
begin

  inherited;
end;

procedure TJSONVirtualDto.DoInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>;
  out Result: TValue);
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

function TJSONVirtualDto.GetIdentity: string;
begin
  Result := FObservable.GetIdentity;
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

function TJSONVirtualDto.IsPaused: boolean;
begin
  Result := FObservable.IsPaused;
end;

procedure TJSONVirtualDto.Pause;
begin
  FObservable.Pause;
end;

procedure TJSONVirtualDto.ProcessDtoAttributes(const PIID: PTypeInfo);
var
  Context: TRttiContext;
  RttiType: TRttiType;
begin
  inherited;

  Context := TRttiContext.Create;

  RttiType := Context.GetType(PIID);

  // process all methods (and their attributes)
  TCollections.CreateList<TRttiMethod>(RttiType.GetMethods).ForEach(
    procedure(const Method: TRttiMethod)
    begin
      ProcessMethod(Method);
    end);

  // set remaining methods to rows affected or colgetters
  FRecordMethods.Values
    .Where(
      function(const Value: TJSONDTOMethodDescriptor): Boolean
      begin
        Result := Value.Category = mcNone;
      end)
    .ForEach(
      procedure(const Value: TJSONDTOMethodDescriptor)
      begin
        Value.Category := mcColGetter;
      end);
end;

procedure TJSONVirtualDto.ProcessMethod(const Method: TRttiMethod);
var
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
  TCollections.CreateList<TCustomAttribute>(Method.GetAttributes)
    .Where(function(const Attribute: TCustomAttribute): Boolean
      begin
        Result := Attribute is ColumnAttribute;
      end)
    .ForEach(procedure(const Attribute: TCustomAttribute)
      begin
        MethodDesc.MappedName := ColumnAttribute(Attribute).Line;
      end);
end;

procedure TJSONVirtualDto.RegisterObserver(const Observer: IObserver);
begin
  FObservable.RegisterObserver(Observer);
end;

procedure TJSONVirtualDto.Resume(const AndBroadcast: string);
begin
  FObservable.Resume(AndBroadcast);
end;

procedure TJSONVirtualDto.UnregisterObserver(const Observer: IObserver);
begin
  FObservable.UnregisterObserver(Observer);
end;

{ JSONUnmarshaller }

class function JSONUnmarshaller.ToReadonlyList(
  const JSONString: string;
  const TypeInfo: PTypeInfo;
  const ConfigurationName: string): TValue;
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
  else if ElementRttiType.TypeKind = tkClass then
    Result := JSONUnmarshaller.ToReadonlyListOfObjects(JSONString, TypeInfo, ElementRttiType.Handle)
  else if ElementRttiType.TypeKind = tkEnumeration then
    Result := JSONUnmarshaller.ToReadonlyListOfEnums(JSONString, TypeInfo, ConfigurationName)
  else
    Result := JSONUnmarshaller.ToReadonlyListOfPrimitives(JSONString, ElementRttiType.Handle, ConfigurationName)
end;

class function JSONUnmarshaller.ToReadonlyListOfEnums(
  const JSONString: string;
  const TypInfo: PTypeInfo;
  const ConfigurationName: string): TValue;
var
  JsonArray: IShared<TJSONArray>;
  ValuesArray: TArray<string>;
  Index: Integer;
  IntList: IReadOnlyList<Integer>;
begin
  if not JSONString.IsEmpty then
  begin
    JsonArray := Shared.Make(TJSONObject.ParseJSONValue(JSONString) as TJSONArray);

    SetLength(ValuesArray, JsonArray.Count);
    for Index := 0 to JsonArray.Count - 1 do
      ValuesArray[Index] := JsonArray.Items[Index].GetValue<string>;
  end;

  IntList := CreateReadonlyListAsValue(TypeInfo(Integer), ValuesArray, ConfigurationName).AsType<IReadonlyList<Integer>>;
  TValue.Make(@IntList, TypInfo, Result);
end;

class function JSONUnmarshaller.ToReadonlyListOfInterfaces(
  const JSONString: string;
  const TypeInfo: PTypeInfo;
  const ElementTypeInfo: PTypeInfo): TValue;
var
  JsonArray: IShared<TJSONArray>;
  IntfList: IReadOnlyList<IInterface>;
begin
  JsonArray := Shared.Make(TJSONObject.ParseJSONValue(JSONString) as TJSONArray);

  IntfList := TJsonArrayAsReadonlyInterfaceList<IInterface>.Create(JSONArray, ElementTypeInfo);
  TValue.Make(@IntfList, TypeInfo, Result);
end;

class function JSONUnmarshaller.ToReadonlyListOfObjects(
  const JSONString: string;
  const TypeInfo: PTypeInfo;
  const ElementTypeInfo: PTypeInfo): TValue;
var
  JsonArray: IShared<TJSONArray>;
  ObjList: IReadOnlyList<TObject>;
begin
  JsonArray := Shared.Make(TJSONObject.ParseJSONValue(JSONString) as TJSONArray);

  ObjList := TJsonArrayAsReadonlyObjectList<TObject>.Create(JSONArray, ElementTypeInfo);
  TValue.Make(@ObjList, TypeInfo, Result);
end;

class function JSONUnmarshaller.ToReadonlyListOfPrimitives(
  const JSONString: string;
  const ElementTypeInfo: PTypeInfo;
  const ConfigurationName: string): TValue;
var
  JsonArray: IShared<TJSONArray>;
  ValuesArray: TArray<string>;
  Index: Integer;
begin
  if not JSONString.IsEmpty then
  begin
    JsonArray := Shared.Make(TJSONObject.ParseJSONValue(JSONString) as TJSONArray);

    SetLength(ValuesArray, JsonArray.Count);
    for Index := 0 to JsonArray.Count - 1 do
      ValuesArray[Index] := JsonArray.Items[Index].GetValue<string>;
  end;

  Result := CreateReadonlyListAsValue(ElementTypeInfo, ValuesArray, ConfigurationName);
end;

class function JSONUnmarshaller.ToRecord(
  const JSONString: string;
  const TypeInfo: PTypeInfo;
  const ConfigurationName: string): TValue;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Method: TRttiMethod;
  Field: TRttiField;
  Param: TValue;
  JSONObject: IShared<TJSONObject>;
  Value: TValue;
  Mapping: MappingsUtilities.TJSONMarshallingMapping;
begin
  if MappingsUtilities.TryGetType(TypeInfo, Mapping, ConfigurationName) then
  begin
    Result := Mapping.&To(JSONString, TypeInfo);
    Exit;
  end;

  JSONObject := Shared.Make(TJSONObject.ParseJSONValue(JSONString) as TJSONObject);
  TValue.Make(nil, TypeInfo, Value);

  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(TypeInfo);
  with JSONObject.GetEnumerator do
  begin
    while MoveNext do
    begin
      Prop := RttiType.GetProperty(GetCurrent.JsonString.Value);
      if Assigned(Prop) and Prop.IsWritable then
        Prop.SetValue(Value.GetReferenceToRawData, JSONUnmarshaller.To(GetCurrent.JsonValue.ToJSON, Prop.PropertyType.Handle, ConfigurationName))
      else
      begin
        Method := RttiType.GetMethod(Format('Set%s', [GetCurrent.JsonString.Value]));
        if Assigned(Method) and (Method.MethodKind = mkProcedure) and (Length(Method.GetParameters) = 1) then
        begin
          Param := JSONUnmarshaller.To(GetCurrent.JsonValue.ToJSON, Method.GetParameters[0].ParamType.Handle, ConfigurationName);
          Method.Invoke(Value, [Param]);
        end
        else
        begin
          Field := RttiType.GetField(GetCurrent.JsonString.Value);
          if Assigned(Field) then
            Field.SetValue(Value.GetReferenceToRawData, JSONUnmarshaller.To(GetCurrent.JsonValue.ToJSON, Field.FieldType.Handle, ConfigurationName));
        end;
      end;
    end;
    Free;
  end;

  Result := Value;
end;

class function JSONUnmarshaller.ToInterfaceAsValue(
  const JSONString: string;
  const TypeInfo: PTypeInfo;
  const ConfigurationName: string): TValue;
var
  Intf: IInterface;
begin
  Intf := JSONUnmarshaller.ToInterface(JSONString, TypeInfo, ConfigurationName);
  TValue.Make(@Intf, TypeInfo, Result);
end;

class function JSONUnmarshaller.&To<T>(
  const JSONString: string;
  const ConfigurationName: string): T;
var
  Context: TRttiContext;
  RttiType: TRttiType;
begin
  Context := TRttiContext.Create;
  RttiType := Context.GetType(TypeInfo(T));
  Result := JSONUnmarshaller.To(JSONString, RttiType.Handle, ConfigurationName).AsType<T>;
end;

class function JSONUnmarshaller.ToEnumeration(
  const Value: string;
  const TypeInfo: PTypeInfo;
  const ConfigurationName: string): TValue;
var
  Mapping: MappingsUtilities.TJSONMarshallingMapping;
begin
  if MappingsUtilities.TryGetEnumeratives(Mapping, ConfigurationName) then
    Result := Mapping.&To(Value, TypeInfo);
end;

class function JSONUnmarshaller.ToPrimitive(
  const Value: string;
  const TypeInfo: PTypeInfo;
  const ConfigurationName: string): TValue;
var
  Mapping: MappingsUtilities.TJSONMarshallingMapping;
begin
  if MappingsUtilities.TryGetType(TypeInfo, Mapping, ConfigurationName) then
    Result := Mapping.&To(Value, TypeInfo);
end;

class function JSONUnmarshaller.&To(
  const JSONString: string;
  const TypeInfo: PTypeInfo;
  const ConfigurationName: string): TValue;
var
  Context: TRttiContext;
  RttiType: TRttiType;
begin
  Context := TRttiContext.Create;
  RttiType := Context.GetType(TypeInfo);

  try
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
          Result := JSONUnmarshaller.ToPrimitive(JSONString, RttiType.Handle, ConfigurationName)
        else
          Result := JSONUnmarshaller.ToEnumeration(JSONString, RttiType.Handle, ConfigurationName);
      end;
      tkClass: Result := JSONUnmarshaller.ToObject(JSONString, TypeInfo, ConfigurationName);
      tkInterface: begin
        if RttiType.QualifiedName.ToLower.StartsWith(ArrayInterfaceName) then
          Result := JSONUnmarshaller.ToReadonlyList(JSONString, TypeInfo, ConfigurationName)
        else
          Result := JSONUnmarshaller.ToInterfaceAsValue(JSONString, TypeInfo, ConfigurationName);
      end;

      tkChar,
      tkString,
      tkWChar,
      tkLString,
      tkWString,
      tkUString,
      tkFloat,
      tkInteger,
      tkInt64: Result := JSONUnmarshaller.ToPrimitive(JSONString, RttiType.Handle, ConfigurationName);

      tkRecord,
      tkMRecord: begin
        if string(TypeInfo.Name).ToLower.StartsWith(NullableName) or
           string(TypeInfo.Name).ToLower.Equals(GuidName)then
          Result := JSONUnmarshaller.ToPrimitive(JSONString, RttiType.Handle, ConfigurationName)
        else
          Result := JSONUnmarshaller.ToRecord(JSONString, RttiType.Handle, ConfigurationName);
      end;
    end;
  except
    on E: Exception do
      raise EJSONUnmarshaller.Create(E.Message);
  end;
end;

class function JSONUnmarshaller.CreateList<T>(
  const ValuesArray: array of string;
  const ConfigurationName: string): TValue;
var
  List: IList<T>;
  ConvertedArray: TArray<T>;
  Index: Integer;
begin
  SetLength(ConvertedArray, Length(ValuesArray));

  for Index := 0 to Length(ValuesArray) - 1 do
    ConvertedArray[Index] := JSONUnmarshaller.&To<T>(ValuesArray[Index], ConfigurationName);

  List := TCollections.CreateList<T>(ConvertedArray);
  Result := TValue.From<IReadOnlyList<T>>(List as IReadOnlyList<T>);
end;

class function JSONUnmarshaller.CreateReadonlyListAsValue(
  const ElementTypeInfo: PTypeInfo;
  const ValuesArray: array of string;
  const ConfigurationName: string): TValue;
var
  Context: TRttiContext;
  RttiType: TRttiType;
begin
  Context := TRttiContext.Create;

  RttiType := Context.GetType(ElementTypeInfo);

  if RttiType.QualifiedName.ToLower = StringName then
    Result := CreateList<string>(ValuesArray, ConfigurationName)
  else if RttiType.QualifiedName.ToLower = IntegerName then
    Result := CreateList<Integer>(ValuesArray, ConfigurationName)
  else if RttiType.QualifiedName.ToLower = Int64Name then
    Result := CreateList<Int64>(ValuesArray, ConfigurationName)
  else if RttiType.QualifiedName.ToLower = GuidName then
    Result := CreateList<TGuid>(ValuesArray, ConfigurationName)
  else if RttiType.QualifiedName.ToLower = BooleanName then
    Result := CreateList<Boolean>(ValuesArray, ConfigurationName)
  else if RttiType.QualifiedName.ToLower = DatetimeName then
    Result := CreateList<TDateTime>(ValuesArray, ConfigurationName)
  else if RttiType.QualifiedName.ToLower = DoubleName then
    Result := CreateList<Double>(ValuesArray, ConfigurationName)
  else if RttiType.QualifiedName.ToLower = CurrencyName then
    Result := CreateList<Currency>(ValuesArray, ConfigurationName)
  else if RttiType.QualifiedName.ToLower = ExtendedName then
    Result := CreateList<Extended>(ValuesArray, ConfigurationName)
  else
    raise EJSONVirtualDto.CreateFmt('%s is not a supported type for TJSONVirtualDto arrays.', [RttiType.QualifiedName]);
end;

class function JSONUnmarshaller.ToInterface(
  const JSONString: string;
  const TypeInfo: PTypeInfo;
  const ConfigurationName: string): IInterface;
var
  RInterface: IInterface;
  JSONObject: IShared<TJSONObject>;
begin
  JSONObject := Shared.Make(TJSONObject.ParseJSONValue(JSONString) as TJSONObject);
  Supports(TJSONVirtualDto.Create(TypeInfo, JSONObject, ConfigurationName), GetTypeData(TypeInfo)^.Guid, RInterface);
  Result := RInterface;
end;

class function JSONUnmarshaller.ToObject(
  const JSONString: string;
  const TypeInfo: PTypeInfo;
  const ConfigurationName: string): TValue;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  InstanceType: TRttiInstanceType;
  Prop: TRttiProperty;
  Method: TRttiMethod;
  Param: TValue;
  JSONObject: IShared<TJSONObject>;
  Mapping: MappingsUtilities.TJSONMarshallingMapping;
begin
  if MappingsUtilities.TryGetType(TypeInfo, Mapping, ConfigurationName) then
  begin
    Result := Mapping.&To(JSONString, TypeInfo);
    Exit;
  end;

  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(TypeInfo);
  JSONObject := Shared.Make(TJSONObject.ParseJSONValue(JSONString) as TJSONObject);
  InstanceType := RttiType.AsInstance;
  Result := InstanceType.GetMethod('Create').Invoke(InstanceType.MetaclassType, []).Convert(TypeInfo);
  with JSONObject.GetEnumerator do
  begin
    while MoveNext do
    begin
      Prop := RttiType.GetProperty(GetCurrent.JsonString.Value);
      if Assigned(Prop) and Prop.IsWritable then
        Prop.SetValue(Result.AsObject, JSONUnmarshaller.To(GetCurrent.JsonValue.ToJSON, Prop.PropertyType.Handle, ConfigurationName))
      else
      begin
        Method := RttiType.GetMethod(Format('Set%s', [GetCurrent.JsonString.Value]));
        if Assigned(Method) and (Method.MethodKind = mkProcedure) and (Length(Method.GetParameters) = 1) then
        begin
          Param := JSONUnmarshaller.To(GetCurrent.JsonValue.ToJSON, Method.GetParameters[0].ParamType.Handle, ConfigurationName);
          Method.Invoke(Result.AsObject, [Param]);
        end;
      end;
    end;
    Free;
  end;
end;

{ JSONMarshaller }

class function JSONMarshaller.From(
  const Value: TValue;
  const TypInfo: PTypeInfo;
  const ConfigurationName: string): string;
var
  MarshalledValue: Shared<TJSONValue>;
begin
  Result := '';
  try
    MarshalledValue := InternalFrom(Value, TypInfo, ConfigurationName);
  except
    on E: Exception do
      raise EJSONMarshaller.Create(E.Message);
  end;
  if Assigned(MarshalledValue.Value) then
    Result := MarshalledValue.Value.ToJSON;
end;

class function JSONMarshaller.From<T>(
  const Value: T;
  const ConfigurationName: string): string;
begin
  Result := JSONMarshaller.From(TValue.From<T>(Value), TypeInfo(T), ConfigurationName);
end;

class function JSONMarshaller.FromEnumeration(const Value: TValue; const ConfigurationName: string): Nullable<string>;
var
  Mapping: MappingsUtilities.TJSONMarshallingMapping;
begin
  if MappingsUtilities.TryGetEnumeratives(Mapping, ConfigurationName) then
    Result := Mapping.&From(Value);
end;

class function JSONMarshaller.FromInterface(
  const Value: TValue;
  const TypInfo: PTypeInfo;
  const ConfigurationName: string): Nullable<string>;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  JSONObject: IShared<TJSONObject>;
  LValue: TValue;
begin
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(TypInfo);

  JSONObject := Shared.Make(TJSONObject.Create);

  LValue := Value;

  TCollections.CreateList<TRttiMethod>(RttiType.GetMethods)
    .Where(function(const Method: TRttiMethod): Boolean
      begin
        Result := (Method.Visibility in [mvPublic]) and (Method.MethodKind = mkFunction) and (Length(Method.GetParameters) = 0);
      end)
    .ForEach(procedure(const Method: TRttiMethod)
      var
        ReturnValue: TValue;
        MarshalledValue: TJSONValue;
      begin
        try
          ReturnValue := Method.Invoke(LValue, []);

          MarshalledValue := JSONMarshaller.InternalFrom(ReturnValue, Method.ReturnType.Handle, ConfigurationName);
          if Assigned(MarshalledValue) then
            JSONObject.AddPair(Method.Name, MarshalledValue)
          else
            JSONObject.AddPair(Method.Name, TJSONNull.Create);
        except
        end;
      end);

  TCollections.CreateList<TRttiProperty>(RttiType.GetProperties)
    .Where(function(const Prop: TRttiProperty): Boolean
      begin
        Result := (Prop.Visibility in [mvPublic]) and Prop.IsReadable;
      end)
    .ForEach(procedure(const Prop: TRttiProperty)
      var
        ReturnValue: TValue;
        MarshalledValue: TJSONValue;
      begin
        try
          ReturnValue := Prop.GetValue(LValue.AsInterface);

          MarshalledValue := JSONMarshaller.InternalFrom(ReturnValue, Prop.PropertyType.Handle, ConfigurationName);
          if Assigned(MarshalledValue) then
            JSONObject.AddPair(Prop.Name, MarshalledValue)
          else
            JSONObject.AddPair(Prop.Name, TJSONNull.Create);
        except
        end;
      end);

  Result := JSONObject.ToJson;
end;

class function JSONMarshaller.FromObject(
  const Value: TObject;
  const TypInfo: PTypeInfo;
  const ConfigurationName: string): Nullable<string>;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  JSONObject: IShared<TJSONObject>;
  Mapping: MappingsUtilities.TJSONMarshallingMapping;
begin
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(TypInfo);

  if MappingsUtilities.TryGetType(TypInfo, Mapping, ConfigurationName) then
  begin
    Result := Mapping.From(Value);
    Exit;
  end;

  JSONObject := Shared.Make(TJSONObject.Create);

  TCollections.CreateList<TRttiMethod>(RttiType.GetMethods)
    .Where(function(const Method: TRttiMethod): Boolean
      begin
        Result := (Method.Visibility in [mvPublished]) and (Method.MethodKind = mkFunction) and (Length(Method.GetParameters) = 0);
      end)
    .ForEach(procedure(const Method: TRttiMethod)
      var
        ReturnValue: TValue;
        MarshalledValue: TJSONValue;
      begin
        ReturnValue := Method.Invoke(Value, []);
        try
          MarshalledValue := JSONMarshaller.InternalFrom(ReturnValue, Method.ReturnType.Handle, ConfigurationName);
          if Assigned(MarshalledValue) then
            JSONObject.AddPair(Method.Name, MarshalledValue)
          else
            JSONObject.AddPair(Method.Name, TJSONNull.Create);
        except
        end;
      end);

  TCollections.CreateList<TRttiProperty>(RttiType.GetProperties)
    .Where(function(const Prop: TRttiProperty): Boolean
      begin
        Result := (Prop.Visibility in [mvPublished]) and Prop.IsReadable;
      end)
    .ForEach(procedure(const Prop: TRttiProperty)
      var
        ReturnValue: TValue;
        MarshalledValue: TJSONValue;
      begin
        ReturnValue := Prop.GetValue(Value);
        try
          MarshalledValue := JSONMarshaller.InternalFrom(ReturnValue, Prop.PropertyType.Handle, ConfigurationName);
          if Assigned(MarshalledValue) then
            JSONObject.AddPair(Prop.Name, MarshalledValue)
          else
            JSONObject.AddPair(Prop.Name, TJSONNull.Create);
        except
        end;
      end);

  Result := JSONObject.ToJson;
end;

class function JSONMarshaller.FromPrimitive(
  const Value: TValue;
  const TypInfo: PTypeInfo;
  const ConfigurationName: string): Nullable<string>;
var
  Mapping: MappingsUtilities.TJSONMarshallingMapping;
begin
  Result := Nullable<string>.Create(Null);
  if MappingsUtilities.TryGetType(TypInfo, Mapping, ConfigurationName) then
    Result := Mapping.From(Value);
end;

class function JSONMarshaller.FromReadonlyList(
  const Value: TValue;
  const TypInfo: PTypeInfo;
  const ConfigurationName: string): string;
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
    Result := JSONMarshaller.FromReadonlyListOfInterfaces(Value, TypInfo, ElementRttiType.Handle, ConfigurationName)
  else if ElementRttiType.TypeKind = tkClass then
    Result := JSONMarshaller.FromReadonlyListOfObjects(Value, TypInfo, ElementRttiType.Handle, ConfigurationName)
  else if ElementRttiType.TypeKind = tkEnumeration then
    Result := JSONMarshaller.FromReadonlyListOfEnums(Value, TypInfo, ConfigurationName)
  else if (ElementRttiType.TypeKind = tkRecord) or
          (ElementRttiType.TypeKind = tkMRecord) then
    Result := JSONMarshaller.FromReadonlyListOfRecords(Value, TypInfo, ElementRttiType.Handle, ConfigurationName)
  else
    Result := JSONMarshaller.FromReadonlyListOfPrimitives(Value, TypInfo, ElementRttiType.Handle, ConfigurationName)
end;

class function JSONMarshaller.FromReadonlyListOfEnums(
  const Value: TValue;
  const TypInfo: PTypeInfo;
  const ConfigurationName: string): string;

  function Convert(const Value: TValue): TJSONValue;
  var
    NullValue: Nullable<string>;
    StrValue: string;
  begin
    NullValue := JSONMarshaller.FromEnumeration(Value, ConfigurationName);
    if not NullValue.HasValue then
      Exit(TJSONNull.Create);

    StrValue := NullValue.Value;

    Result := TJSONObject.ParseJSONValue(StrValue);
  end;

var
  JsonArray: IShared<TJSONArray>;
  Enum: IEnumerable;
begin
  JsonArray := Shared.Make(TJSONArray.Create);

  Enum := (Value.Convert(TypInfo).AsInterface as IEnumerable);
  with Enum.GetEnumerator do
    while MoveNext do
    begin
      JsonArray.AddElement(Convert(Current));
    end;

  Result := JsonArray.ToJSON;
end;

class function JSONMarshaller.FromReadonlyListOfInterfaces(
  const Value: TValue;
  const TypInfo, ElementTypeInfo: PTypeInfo;
  const ConfigurationName: string): string;
var
  JsonArray: IShared<TJSONArray>;
  Enum: IEnumerable;
begin
  JsonArray := Shared.Make(TJSONArray.Create);

  Enum := (Value.Convert(TypInfo).AsInterface as IEnumerable);
  with Enum.GetEnumerator do
    while MoveNext do
      JsonArray.AddElement(TJSONObject.ParseJSONValue(JSONMarshaller.FromInterface(Current, ElementTypeInfo, ConfigurationName)));

  Result := JsonArray.ToJSON;
end;

class function JSONMarshaller.FromReadonlyListOfObjects(
  const Value: TValue;
  const TypInfo: PTypeInfo;
  const ElementTypeInfo: PTypeInfo;
  const ConfigurationName: string): string;
var
  JsonArray: IShared<TJSONArray>;
  Enum: IEnumerable;
begin
  JsonArray := Shared.Make(TJSONArray.Create);

  Enum := (Value.Convert(TypInfo).AsInterface as IEnumerable);
  with Enum.GetEnumerator do
    while MoveNext do
      JsonArray.AddElement(TJSONObject.ParseJSONValue(JSONMarshaller.From(Current, ElementTypeInfo, ConfigurationName)));

  Result := JsonArray.ToJSON;
end;

class function JSONMarshaller.FromReadonlyListOfPrimitives(
  const Value: TValue;
  const TypInfo: PTypeInfo;
  const ElementTypeInfo: PTypeInfo;
  const ConfigurationName: string): string;

  function Convert(const Value: TValue): TJSONValue;
  var
    NullValue: Nullable<string>;
    StrValue: string;
  begin
    NullValue := JSONMarshaller.FromPrimitive(Value, ElementTypeInfo, ConfigurationName);
    if not NullValue.HasValue then
      Exit(TJSONNull.Create);

    StrValue := NullValue.Value;

    if not (string(TypInfo.Name).ToLower.Contains('integer') or
            string(TypInfo.Name).ToLower.Contains('int64') or
            String(TypInfo.Name).ToLower.Contains('smallint') or
            String(TypInfo.Name).ToLower.Contains('extended') or
            String(TypInfo.Name).ToLower.Contains('double') or
            String(TypInfo.Name).ToLower.Contains('currency') or
            String(TypInfo.Name).ToLower.Contains('boolean')) then
      Result := TJSONObject.ParseJSONValue(StrValue.QuotedString('"'))
    else
      Result := TJSONObject.ParseJSONValue(StrValue);
  end;

var
  JsonArray: IShared<TJSONArray>;
  Enum: IEnumerable;
begin
  JsonArray := Shared.Make(TJSONArray.Create);

  Enum := (Value.Convert(TypInfo).AsInterface as IEnumerable);
  if Assigned(Enum) then
  begin
    with Enum.GetEnumerator do
      while MoveNext do
      begin
        JsonArray.AddElement(Convert(Current));
      end;
    Result := JsonArray.ToJSON;
  end;
end;

class function JSONMarshaller.FromReadonlyListOfRecords(
  const Value: TValue;
  const TypInfo: PTypeInfo;
  const ElementTypeInfo: PTypeInfo;
  const ConfigurationName: string): string;
var
  JsonArray: IShared<TJSONArray>;
  Enum: IEnumerable;
begin
  JsonArray := Shared.Make(TJSONArray.Create);

  Enum := (Value.Convert(TypInfo).AsInterface as IEnumerable);
  with Enum.GetEnumerator do
    while MoveNext do
      JsonArray.AddElement(TJSONObject.ParseJSONValue(JSONMarshaller.From(Current, ElementTypeInfo, ConfigurationName)));

  Result := JsonArray.ToJSON;
end;

class function JSONMarshaller.FromRecord(
  const Value: TValue;
  const TypInfo: PTypeInfo;
  const ConfigurationName: string): Nullable<string>;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  JSONObject: IShared<TJSONObject>;
  LValue: TValue;
  Mapping: MappingsUtilities.TJSONMarshallingMapping;
begin
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(TypInfo);

  if MappingsUtilities.TryGetType(TypInfo, Mapping, ConfigurationName) then
  begin
    Result := Mapping.From(Value);
    Exit;
  end;

  JSONObject := Shared.Make(TJSONObject.Create);

  LValue := Value;

  TCollections.CreateList<TRttiMethod>(RttiType.GetMethods)
    .Where(function(const Method: TRttiMethod): Boolean
      begin
        Result := (Method.Visibility in [mvPublic]) and (Method.MethodKind = mkFunction) and (Length(Method.GetParameters) = 0);
      end)
    .ForEach(procedure(const Method: TRttiMethod)
      var
        ReturnValue: TValue;
        MarshalledValue: TJSONValue;
      begin
        ReturnValue := Method.Invoke(LValue, []);
        try
          MarshalledValue := JSONMarshaller.InternalFrom(ReturnValue, Method.ReturnType.Handle, ConfigurationName);
          if Assigned(MarshalledValue) then
            JSONObject.AddPair(Method.Name, MarshalledValue)
          else
            JSONObject.AddPair(Method.Name, TJSONNull.Create);
        except
        end;
      end);

  TCollections.CreateList<TRttiProperty>(RttiType.GetProperties)
    .Where(function(const Prop: TRttiProperty): Boolean
      begin
        Result := (Prop.Visibility in [mvPublic]) and Prop.IsReadable;
      end)
    .ForEach(procedure(const Prop: TRttiProperty)
      var
        ReturnValue: TValue;
        MarshalledValue: TJSONValue;
      begin
        ReturnValue := Prop.GetValue(LValue.GetReferenceToRawData);
        try
          MarshalledValue := JSONMarshaller.InternalFrom(ReturnValue, Prop.PropertyType.Handle, ConfigurationName);
          if Assigned(MarshalledValue) then
            JSONObject.AddPair(Prop.Name, MarshalledValue)
          else
            JSONObject.AddPair(Prop.Name, TJSONNull.Create);
        except
        end;
      end);

  TCollections.CreateList<TRttiField>(RttiType.GetFields)
    .Where(function(const Field: TRttiField): Boolean
      begin
        Result := (Field.Visibility in [mvPublic]);
      end)
    .ForEach(procedure(const Field: TRttiField)
      var
        ReturnValue: TValue;
        MarshalledValue: TJSONValue;
      begin
        ReturnValue := Field.GetValue(LValue.GetReferenceToRawData);
        try
          MarshalledValue := JSONMarshaller.InternalFrom(ReturnValue, Field.FieldType.Handle, ConfigurationName);
          if Assigned(MarshalledValue) then
            JSONObject.AddPair(Field.Name, MarshalledValue)
          else
            JSONObject.AddPair(Field.Name, TJSONNull.Create);
        except
        end;
      end);

  Result := JSONObject.ToJson;
end;

class function JSONMarshaller.InternalFrom(
  const Value: TValue;
  const TypInfo: PTypeInfo;
  const ConfigurationName: string): TJsonValue;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Mapping: MappingsUtilities.TJSONMarshallingMapping;
  MarshalledValue: Nullable<string>;
  FloatValue: Extended;
  FormatSettings: TFormatSettings;
begin
  Result := nil;

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
      begin
        MarshalledValue := JSONMarshaller.FromPrimitive(Value, TypInfo, ConfigurationName);
        if MarshalledValue.HasValue then
          Result := TJSONUnQuotedString.Create(MarshalledValue);
      end
      else if MappingsUtilities.TryGetEnumeratives(Mapping, ConfigurationName) then
        Result := TJSONUnQuotedString.Create(Mapping.From(Value));
    end;
    tkClass: Result := TJSONObject.ParseJSONValue(JSONMarshaller.FromObject(Value.AsObject, TypInfo, ConfigurationName)) as TJSONObject;
    tkInterface: begin
      if RttiType.QualifiedName.ToLower.StartsWith(ArrayInterfaceName) then
        Result := TJSONObject.ParseJSONValue(JSONMarshaller.FromReadonlyList(Value, TypInfo, ConfigurationName)) as TJSONArray
      else
        Result := TJSONObject.ParseJSONValue(JSONMarshaller.FromInterface(Value, TypInfo, ConfigurationName)) as TJSONObject;
    end;

    tkChar,
    tkString,
    tkWChar,
    tkLString,
    tkWString,
    tkUString: begin
      MarshalledValue := JSONMarshaller.FromPrimitive(Value, RttiType.Handle, ConfigurationName);
      if MarshalledValue.HasValue then
        Result := TJSONString.Create(MarshalledValue);
    end;
    tkFloat,
    tkInteger,
    tkInt64: begin
      FormatSettings := TFormatSettings.Create;
      FormatSettings.ThousandSeparator := ',';
      FormatSettings.DecimalSeparator := '.';
      MarshalledValue := JSONMarshaller.FromPrimitive(Value, RttiType.Handle, ConfigurationName);
      if MarshalledValue.HasValue then
        if not TryStrToFloat(MarshalledValue.Value, FloatValue, FormatSettings) then
          Result := TJSONString.Create(MarshalledValue)
        else
          Result := TJSONUnQuotedString.Create(MarshalledValue);
    end;
    tkRecord,
    tkMRecord: begin
      if string(TypInfo.Name).ToLower.StartsWith(NullableName) then
      begin
        MarshalledValue := JSONMarshaller.FromPrimitive(Value, RttiType.Handle, ConfigurationName);
        if MarshalledValue.HasValue then
          if RttiType.QualifiedName.ToLower.Contains(StringName) or
             RttiType.QualifiedName.ToLower.Contains(DateTimeName) or
             RttiType.QualifiedName.ToLower.Contains(GuidName) then
            Result := TJSONString.Create(MarshalledValue)
          else
            Result := TJSONUnQuotedString.Create(MarshalledValue);
      end
      else if string(TypInfo.Name).ToLower.StartsWith(GuidName) then
      begin
        MarshalledValue := JSONMarshaller.FromPrimitive(Value, RttiType.Handle, ConfigurationName);
        if MarshalledValue.HasValue then
          Result := TJSONString.Create(MarshalledValue);
      end
      else
      begin
        MarshalledValue := JSONMarshaller.FromRecord(Value, RttiType.Handle, ConfigurationName);
        if MarshalledValue.HasValue then
          Result := TJSONObject.ParseJSONValue(MarshalledValue) as TJSONObject;
      end;
    end;
  end;
end;

{ TJSONDTOMethodDescriptor }

function TJSONVirtualDto.TJSONDTOMethodDescriptor.IsArray: Boolean;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
begin
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(TypeInfo);
  Result := RttiType.QualifiedName.ToLower.StartsWith(ArrayInterfaceName);
end;

end.
