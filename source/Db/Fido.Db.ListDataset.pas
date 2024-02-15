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
unit Fido.Db.ListDataset;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.StrUtils,
  System.TypInfo,
  System.Rtti,
  Data.DB,

  Spring,
  Spring.Collections,

  Fido.Types,
  Fido.DesignPatterns.Observable.Intf,
  Fido.Collections.DeepObservableList.Intf,
  Fido.Exceptions,
  Fido.Collections,
  Fido.Utilities,
  Fido.Db.TypeConverter,
  Fido.Db.VirtualDataset,
  Fido.Db.DatasetFieldAttributes.Intf,
  Fido.Db.DatasetFieldAttributes;

type
  EFidoListDataSetException = class(EFidoException);

  TListDataSet<T: IInvokable> = class(TVirtualDataSet)
  strict private type
    TMethodInfo = record
      FieldName: string;
      IsList: Boolean;
      TypeKind: TTypeKind;
      VariableTypeName: string;
      Handle: PTypeInfo;
    end;
  strict private const
      GETTER_PREFIX = 'GET';
      SETTER_PREFIX = 'SET';
      MAX_TRAVERSEOBJECT = 2;
  strict private
      FEntityFactoryFunc: TFunc<PTypeInfo, TValue>;
      FOriginalDataList: IList<T>;
      FFilteredDataList: IList<T>;
      FStringMaxLength: Integer;
      FOnCheckItemVisibility: Predicate<T>;
      FTraversedTypeInfoMap: IDictionary<PTypeInfo, Integer>;
      FDatasetFieldAttributes: IDatasetFieldAttributes;

    function TryGetGetterMethodInfo(const RttiMeth: TRttiMethod; out MethodInfo: TMethodInfo): Boolean;
    function TryGetSetterMethodInfo(const RttiMeth: TRttiMethod; out MethodInfo: TMethodInfo): Boolean;
    procedure AddFieldDef(const FieldName: string; const FieldType: TFieldType);
    procedure SetDataList(const Value: IList<T>);
    procedure ListChangedEvent(Sender: TObject; const Item: T; Action: TCollectionChangedAction);
    procedure InternalInitFieldDefsObjectClass(const TypInfo: PTypeInfo; const Prefix: string);
    procedure DoInitialize(const EntityFactoryFunc: TFunc<PTypeInfo, TValue>);
    function IsInRange(const Index: Integer): Boolean;
    procedure SetOnCheckItemVisibility(const Value: Predicate<T>);
    function GetCurrentEntity: T;
    function GetEntityFieldValue(const Entity: TValue; const Prefix: string; const FieldName: string; out Value: Variant): Boolean; overload;
    procedure RecordToEntity(const RecordNo: Integer); overload;
    procedure RecordToEntity(Entity: TValue; const Prefix: string); overload;
    function InternalDataList: IList<T>;
    function IsFiltered: Boolean;
    procedure FilterDataset;
    procedure OnFieldGetText( Sender: TField; var Text: string; DisplayText: Boolean);
    procedure _OnDeleteRecord(Sender: TCustomVirtualDataset; const Index: Integer); virtual;
    procedure _OnGetRecordCount(Sender: TCustomVirtualDataset; var Count: Integer); virtual;
    procedure _OnFilterRecord(DataSet: TDataSet; var Accept: Boolean); virtual;
    procedure _OnGetFieldValue(Sender: TCustomVirtualDataset; const Field: TField; const Index: Integer; var Value: Variant); virtual;
    procedure _OnPostData(Sender: TCustomVirtualDataset; const Index: Integer); virtual;
    procedure _OnLocate(Sender: TCustomVirtualDataset; const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions; var Index: Integer); virtual;
    procedure _OnLookupValue(Sender: TCustomVirtualDataset; const KeyFields: string; const KeyValues: Variant; const ResultFields: string; var Value: Variant); virtual;
    function GetIsGetterName(const Name: string): boolean;
    function GetMappedName(const Name: string): string;
  protected
    FListFactory: TFunc<IList<T>>;

    procedure SetFiltered(Value: Boolean); override;
  public
    constructor Create( Owner: TComponent; const EntityFactoryFunc: TFunc<PTypeInfo, TValue>); reintroduce; overload;
    constructor Create(Owner: TComponent; const EntityFactoryFunc: TFunc<PTypeInfo, TValue>; const DatasetFieldAttributes: IDatasetFieldAttributes); reintroduce; overload;

    property CurrentEntity: T read GetCurrentEntity;
    property DataList: IList<T> read FOriginalDataList write SetDataList;
    property StringMaxLength: Integer read FStringMaxLength write FStringMaxLength;
    property OnCheckItemVisibility: Predicate<T> read FOnCheckItemVisibility write SetOnCheckItemVisibility;
  end;

  TListDataSetOfObservable<T: IObservable> = class(TListDataSet<T>)
  public
    constructor Create(Owner: TComponent; const EntityFactoryFunc: TFunc<PTypeInfo, TValue>); reintroduce; overload;
  end;

implementation

{ TListDataSet<T> }

procedure TListDataSet<T>.AddFieldDef(
  const FieldName: string;
  const FieldType: TFieldType);
begin
  if FieldType = ftString then
    FieldDefs.Add(FieldName, FieldType, FStringMaxLength)
  else
    FieldDefs.Add(FieldName, FieldType, 0);
end;

constructor TListDataSet<T>.Create(
  Owner: TComponent;
  const EntityFactoryFunc: TFunc<PTypeInfo, TValue>;
  const DatasetFieldAttributes: IDatasetFieldAttributes);
begin
  inherited Create(Owner);

  FDatasetFieldAttributes := Utilities.CheckNotNullAndSet(DatasetFieldAttributes, 'FieldViewAttributes');

  FListFactory :=
    function: IList<T>
    begin
      Result := Spring.Collections.TCollections.CreateList<T>();
    end;

  FStringMaxLength := 255;
  FOnCheckItemVisibility := nil;
  FTraversedTypeInfoMap := Spring.Collections.TCollections.CreateDictionary<PTypeInfo, Integer>;

  OnDeleteRecord := _OnDeleteRecord;
  OnGetRecordCount := _OnGetRecordCount;
  OnFilterRecord := _OnFilterRecord;
  OnGetFieldValue := _OnGetFieldValue;
  OnPostData := _OnPostData;
  OnLocate := _OnLocate;
  OnLookupValue := _OnLookupValue;

  DoInitialize(Utilities.CheckNotNullAndSet<TFunc<PTypeInfo, TValue>>(EntityFactoryFunc, 'EntityFactoryFunc'));
end;

constructor TListDataSet<T>.Create(
  Owner: TComponent;
  const EntityFactoryFunc: TFunc<PTypeInfo, TValue>);
begin
  Create(Owner, EntityFactoryFunc, TDatasetFieldAttributes.Create);
end;

function TListDataSet<T>.InternalDataList: IList<T>;
begin
  if IsFiltered then
    Result := FFilteredDataList
  else
    Result := FOriginalDataList;
end;

procedure TListDataSet<T>.DoInitialize(const EntityFactoryFunc: TFunc<PTypeInfo, TValue>);
begin
  FOriginalDataList := FListFactory();
  FOriginalDataList.OnChanged.Add(ListChangedEvent);
  FFilteredDataList := FListFactory();
  FEntityFactoryFunc := EntityFactoryFunc;
  InternalInitFieldDefsObjectClass(TypeInfo(T), '');
  FTraversedTypeInfoMap.Clear;
  CreateFields;
  SetDataList(FOriginalDataList);
end;

procedure TListDataSet<T>.FilterDataset;
begin
  OnFilterRecord := _OnFilterRecord; //Set an empty event
  if not(IsFiltered) then
    Exit;

  FFilteredDataList.Clear;
  FFilteredDataList.InsertRange(0, FOriginalDataList.Where(FOnCheckItemVisibility));
end;

function TListDataSet<T>.GetCurrentEntity: T;
begin
  if Active and not IsEmpty and (RecNo > 0) and (RecNo <= InternalDataList.Count) then
    Result := InternalDataList[RecNo - 1]
  else
    Result := nil;
end;

function TListDataSet<T>.GetEntityFieldValue(
  const Entity: TValue;
  const Prefix: string;
  const FieldName: string;
  out Value: Variant): Boolean;
var
  Context: TRttiContext;
  LType: TRttiType;
  LMethod: TRttiMethod;
  LResult: Boolean;
  LValue: Variant;
  LEntity: TValue;
begin
  LResult := False;
  LValue := Null;
  Context := TRttiContext.Create;
  LEntity := Entity;

  LType := Context.GetType(Entity.TypeInfo);
  if Length(LType.GetDeclaredProperties) <> 0 then
    Spring.Collections.TCollections.CreateList<TRttiProperty>(LType.GetDeclaredProperties).ForEach(
      Procedure(const LProp: TRttiProperty)
      var
        LVar: TValue;
        LField: TField;
        DataTypeDescriptor: TDataTypeDescriptor;
      begin
        if LProp.PropertyType.TypeKind = tkClass then
        begin
          if ContainsText(FieldName, Prefix + LProp.Name + '.') then
          begin
            LVar := LProp.GetValue(LEntity.AsPointer);
            if not LVar.IsEmpty then
              LResult := GetEntityFieldValue(LVar, Prefix + LProp.Name + '.', FieldName, LValue);
          end;
        end
        else
        begin
          LField := Fields.FindField(Prefix + LProp.Name);
          if (LField <> nil) and SameText(FieldName, Prefix + LProp.Name) then
          begin
            LResult := True;
            if DataTypeConverter.GotDescriptor(LProp.PropertyType, DataTypeDescriptor) then
              LValue := DataTypeDescriptor.GetAsVariant(LProp.GetValue(LEntity.AsObject))
            else
              LValue := LProp.GetValue(LEntity.AsObject).AsVariant;
          end;
        end;
      end)
  else
    Spring.Collections.TCollections.CreateList<TRttiMethod>(LType.GetDeclaredMethods).ForEach(
      procedure(const LMethod: TRttiMethod)
      var
        LVar: TValue;
        LField: TField;
        LMethodInfo: TMethodInfo;
        DataTypeDescriptor: TDataTypeDescriptor;
      begin
        if TryGetGetterMethodInfo(LMethod, LMethodInfo) then
        begin
          LField := Fields.FindField(Prefix + LMethodInfo.FieldName);
          LVar := LMethod.Invoke(LEntity, []);
          if LVar.IsInterface and
             ContainsText(FieldName, Prefix + LMethodInfo.FieldName + '.') and
             not LVar.IsEmpty then
            LResult := GetEntityFieldValue(LVar, Prefix + LMethodInfo.FieldName + '.', FieldName, LValue)
          else if (LField <> nil) and SameText(FieldName, Prefix + LMethodInfo.FieldName) then
          begin
            LResult := True;
            if DataTypeConverter.GotDescriptor(LMethodInfo.VariableTypeName, DataTypeDescriptor) then
              LValue := DataTypeDescriptor.GetAsVariant(LVar)
            else
              LValue := LVar.AsVariant;
          end;
        end;
      end);

  Result := LResult;
  Value := LValue;
end;

procedure TListDataSet<T>.InternalInitFieldDefsObjectClass(
  const TypInfo: PTypeInfo;
  const Prefix: string);
var
  Context: TRttiContext;
  LRttiType: TRttiType;
begin
  Context := TRttiContext.Create();

  LRttiType := Context.GetType(TypInfo);

  if Length(LRttiType.GetDeclaredProperties) <> 0 then
    Spring.Collections.TCollections.CreateList<TRttiProperty>(LRttiType.GetDeclaredProperties).ForEach(
      procedure(const LRttiProp: TRttiProperty)
      var
        LTraverseCount: Integer;
        DataTypeDescriptor: TDataTypeDescriptor;
      begin
        //Ensures the fields are added just once
        if (FieldDefs.IndexOf(Prefix + LRttiProp.Name) <> -1) then
          Exit;

        // 1. process nested classes
        if LRttiProp.PropertyType.TypeKind = tkClass then
        begin
          LTraverseCount := FTraversedTypeInfoMap.GetValueOrDefault(LRttiProp.PropertyType.AsInstance.Handle);
          if LTraverseCount < MAX_TRAVERSEOBJECT - 1 then
          begin
            FTraversedTypeInfoMap[LRttiProp.PropertyType.AsInstance.Handle] := LTraverseCount + 1;
            InternalInitFieldDefsObjectClass(LRttiProp.PropertyType.AsInstance.Handle,
                                             Prefix + LRttiProp.Name + '.');
          end
          else
            FTraversedTypeInfoMap[LRttiProp.PropertyType.AsInstance.Handle] := LTraverseCount - 1;
        end
        // 2. try framework-approved basic datatypes handled by DataTypeConverter
        else if DataTypeConverter.GotDescriptor(LRttiProp.PropertyType, DataTypeDescriptor) then
          AddFieldDef(Prefix + LRttiProp.Name, DataTypeDescriptor.FieldType);
      end)
  else
    Spring.Collections.TCollections.CreateList<TRttiMethod>(LRttiType.GetDeclaredMethods).ForEach(
      procedure(const LRttiMeth: TRttiMethod)
      var
        LMethodInfo: TMethodInfo;
        LTraverseCount: Integer;
        DataTypeDescriptor: TDataTypeDescriptor;
      begin
        // don't even try to analyse if field already done or doesn't have a getter or setter
        if not (TryGetGetterMethodInfo(LRttiMeth, LMethodInfo) or
          TryGetSetterMethodInfo(LRttiMeth, LMethodInfo)) or
          (FieldDefs.IndexOf(Prefix + LMethodInfo.FieldName) <> -1) then
          Exit;

        if LMethodInfo.IsList then
          Exit
        // process nested interfaces
        else if LMethodInfo.TypeKind = tkInterface then
        begin
          LTraverseCount := FTraversedTypeInfoMap.GetValueOrDefault(LMethodInfo.Handle);
          if LTraverseCount < MAX_TRAVERSEOBJECT - 1 then
          begin
            FTraversedTypeInfoMap[LMethodInfo.Handle] := LTraverseCount + 1;
            InternalInitFieldDefsObjectClass(LMethodInfo.Handle, Prefix + LMethodInfo.FieldName + '.');
          end
          else
            FTraversedTypeInfoMap[LMethodInfo.Handle] := LTraverseCount - 1;
        end
        // else try standard, supported types
        else if DataTypeConverter.GotDescriptor(LMethodInfo.VariableTypeName, DataTypeDescriptor) then
          AddFieldDef(Prefix + LMethodInfo.FieldName, DataTypeDescriptor.FieldType);
      end);
end;

function TListDataSet<T>.IsFiltered: Boolean;
begin
  Result := Filtered and Assigned(FOnCheckItemVisibility);
end;

function TListDataSet<T>.IsInRange(const Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < InternalDataList.Count);
end;

procedure TListDataSet<T>.ListChangedEvent(
  Sender: TObject;
  const Item: T;
  Action: TCollectionChangedAction);
begin
  if State in [dsEdit, dsInsert] then
    Exit;

  DisableControls;
  case Action of
    caAdded,
    caRemoved,
    caExtracted,
    caReplaced,
    caMoved,
    caReset: begin
      FilterDataset;
      if Active then
        Refresh;
    end;
    caChanged: begin
      FilterDataset;
      if Active then
        Refresh;
    end;
  end;
  EnableControls;
end;

procedure TListDataSet<T>.OnFieldGetText(
  Sender: TField;
  var Text: string;
  DisplayText: Boolean);
var
  Precision: Integer;
begin
  DisplayText :=
    (Sender is TFloatField) or
    (Sender is TSingleField) or
    (Sender is TCurrencyField) or
    (Sender is TExtendedField) or
    (Sender is TBCDField) or
    (Sender is TFMTBCDField);
  if not(DisplayText) then
    Exit;

  if Sender is TCurrencyField then
    Precision := (Sender as TCurrencyField).Precision
  else if Sender is TFloatField then
    Precision := (Sender as TFloatField).Precision
  else if Sender is TSingleField then
    Precision := (Sender as TSingleField).Precision
  else if Sender is TExtendedField then
    Precision := (Sender as TExtendedField).Precision
  else if Sender is TBCDField then
    Precision := (Sender as TBCDField).Precision
  else if Sender is TFMTBCDField then
    Precision := (Sender as TFMTBCDField).Precision;

  Text := FloatToStrF(Sender.Value, ffNumber, 35, Precision);
end;

procedure TListDataSet<T>.RecordToEntity(const RecordNo: Integer);
begin
  RecordToEntity(TValue.From<T>(InternalDataList.Items[RecordNo]), '');
  FTraversedTypeInfoMap.Clear;
end;

procedure TListDataSet<T>.RecordToEntity(
  Entity: TValue;
  const Prefix: string);
var
  Context: TRttiContext;
  LTypes: TRttiType;
//  LMethod: TRttiMethod;
//  LProp: TRttiProperty;
//  Field: TField;
//  LVal: TValue;
//  LSetterMethod: TRttiMethod;
//  LMethodInfo: TMethodInfo;
//  LFieldName: string;
//  LTraverseCount: Integer;
//  DataTypeDescriptor: TDataTypeDescriptor;
begin
  Context := TRttiContext.Create;

  LTypes := Context.GetType(Entity.TypeInfo);
  if Length(LTypes.GetDeclaredProperties) <> 0 then
    Spring.Collections.TCollections.CreateList<TRttiProperty>(LTypes.GetDeclaredProperties).ForEach(
      procedure(const LProp: TRttiProperty)
      var
        Field: TField;
        LVal: TValue;
        LTraverseCount: Integer;
        LMethodInfo: TMethodInfo;
        DataTypeDescriptor: TDataTypeDescriptor;
      begin
        Field := Fields.FindField(Prefix + LProp.Name);
        if (LProp.PropertyType.TypeKind = tkClass) then
        begin
          LVal := LProp.GetValue(Entity.AsPointer);
          if LVal.IsEmpty then
            LVal := FEntityFactoryFunc(LProp.PropertyType.Handle);
          LTraverseCount := FTraversedTypeInfoMap.GetValueOrDefault(LMethodInfo.Handle);
          if LTraverseCount < MAX_TRAVERSEOBJECT - 1 then
          begin
            FTraversedTypeInfoMap[LMethodInfo.Handle] := LTraverseCount + 1;
            RecordToEntity(LVal, Prefix + LProp.Name + '.');
          end;
        end
        else if (Field <> nil) and LProp.IsWritable then
        begin
          if DataTypeConverter.GotDescriptor(LProp.PropertyType, DataTypeDescriptor) then
            LProp.SetValue(Entity.AsObject, DataTypeDescriptor.GetFromVariant(Field.AsVariant))
          else
            LProp.SetValue(Entity.AsObject, TValue.From<variant>(Field.Value));
        end;
      end)
  else
    Spring.Collections.TCollections.CreateList<TRttiMethod>(LTypes.GetDeclaredMethods).ForEach(
      procedure(const LMethod: TRttiMethod)
      var
        LMethodInfo: TMethodInfo;
        Field: TField;
        DataTypeDescriptor: TDataTypeDescriptor;
        LVal: TValue;
        LFieldName: string;
        LTraverseCount: Integer;
      begin
        if TryGetSetterMethodInfo(LMethod, LMethodInfo) and
           (LMethodInfo.TypeKind <> tkInterface) then
        begin
          Field := Fields.FindField(Prefix + LMethodInfo.FieldName);

          if (Field <> nil) then
          begin
            if DataTypeConverter.GotDescriptor(LMethodInfo.VariableTypeName, DataTypeDescriptor) then
              LMethod.Invoke(Entity, [DataTypeDescriptor.GetFromVariant(Field.AsVariant)])
            else
              LMethod.Invoke(Entity, [TValue.From<variant>(Field.Value)]);
          end;
        end
        else if TryGetGetterMethodInfo(LMethod, LMethodInfo) and
                (LMethodInfo.TypeKind = tkInterface) then
        begin
          if LMethodInfo.IsList then
            Exit
          else
          begin
            LVal := LMethod.Invoke(Entity, []);
            if LVal.IsEmpty then
            begin
              LVal := FEntityFactoryFunc(LMethodInfo.Handle);
              LFieldName := LMethodInfo.FieldName;

              Spring.Collections.TCollections.CreateList<TRttiMethod>(LTypes.GetDeclaredMethods).ForEach(
                procedure(const LSetterMethod: TRttiMethod)
                begin
                  if TryGetSetterMethodInfo(LSetterMethod, LMethodInfo) and
                     (LMethodInfo.TypeKind = tkInterface) and
                     (LMethodInfo.FieldName = LFieldName) then
                    LSetterMethod.Invoke(Entity, [LVal]);
                end);
            end;
            LTraverseCount := FTraversedTypeInfoMap.GetValueOrDefault(LMethodInfo.Handle);
            if LTraverseCount < MAX_TRAVERSEOBJECT - 1 then
            begin
              FTraversedTypeInfoMap[LMethodInfo.Handle] := LTraverseCount + 1;
              RecordToEntity(LVal, Prefix + LMethodInfo.FieldName + '.');
            end;
          end;
        end;
      end);
end;

procedure TListDataSet<T>._OnDeleteRecord(
  Sender: TCustomVirtualDataset;
  const Index: Integer);
var
  Item: T;
  OriginalIndex: Integer;
begin
  if IsInRange(Index) then
  begin
    if IsFiltered then
    begin
      Item := InternalDataList[Index];
      OriginalIndex := FOriginalDataList.IndexOf(Item);
      if OriginalIndex <> -1 then
        FOriginalDataList.Delete(OriginalIndex);
    end
    else
      FOriginalDataList.Delete(Index);
  end;
end;

procedure TListDataSet<T>._OnFilterRecord(
  DataSet: TDataSet;
  var Accept: Boolean);
begin
end;

procedure TListDataSet<T>._OnGetFieldValue(
  Sender: TCustomVirtualDataset;
  const Field: TField;
  const Index: Integer;
  var Value: Variant);
begin
  Value := Null;

  if IsInRange(Index) then
  begin
    if not GetEntityFieldValue(TValue.From<T>(InternalDataList.Items[Index]), '', Field.FieldName, Value) then
      raise EFidoListDataSetException.CreateFmt('Field "%s" not found.', [Field.FieldName]);
  end
  else
    raise EFidoListDataSetException.Create('Index out of range.');
end;

procedure TListDataSet<T>._OnGetRecordCount(
  Sender: TCustomVirtualDataset;
  var Count: Integer);
begin
  Count := InternalDataList.Count;
end;

procedure TListDataSet<T>._OnLocate(
  Sender: TCustomVirtualDataset;
  const KeyFields: string;
  const KeyValues: Variant;
  const Options: TLocateOptions;
  var Index: Integer);
var
  List: IList<T>;
  Enumerable: IEnumerable<T>;
  VarArray: TArray<variant>;
  LocalKeyValues: Variant;
  ArrayIndex: Integer;
begin
  Index := -1;
  List := FListFactory;
  List.AddRange(InternalDataList);

  LocalKeyValues := KeyValues;

  if loCaseInsensitive in Options then
    if VarIsArray(LocalKeyValues) then
    begin
      for ArrayIndex := VarArrayLowBound(LocalKeyValues, 1) to VarArrayHighBound(LocalKeyValues, 1) do
        LocalKeyValues[ArrayIndex] := LowerCase(LocalKeyValues[ArrayIndex]);
    end
    else
      LocalKeyValues := LowerCase(LocalKeyValues);

  Enumerable := List.Where(
    function(const Item: T): Boolean
    var
      CurrentValue: Variant;
      Tokenizer: IShared<TStringList>;
      TokenIndex: Integer;
    begin
      if VarIsArray(LocalKeyValues) then
      begin
        Tokenizer := Shared.Make(TStringList.Create);
        Tokenizer.Text := StringReplace(KeyFields, ';', #13#10, [rfReplaceAll]);
        SetLength(VarArray, Tokenizer.Count);
        for TokenIndex := 0 to Tokenizer.Count - 1 do
          if GetEntityFieldValue(TValue.From<T>(Item), '', Tokenizer[TokenIndex], CurrentValue) then
          begin
            VarArray[TokenIndex] := Utilities.IfThen<Variant>(
              function: Boolean
              begin
                Result := loCaseInsensitive in Options;
              end,
              LowerCase(CurrentValue),
              CurrentValue);
            if VarIsStr(VarArray[TokenIndex]) and (loPartialKey in Options) then
              VarArray[TokenIndex] := Copy(VarArray[TokenIndex], 1, Length(LocalKeyValues[TokenIndex]));
          end;
        CurrentValue := VarArray;
      end
      else
      begin
        CurrentValue := null;
        if GetEntityFieldValue(TValue.From<T>(Item), '', KeyFields, CurrentValue) then
          CurrentValue := Utilities.IfThen<Variant>(
              function: Boolean
              begin
                Result := loCaseInsensitive in Options;
              end,
              LowerCase(CurrentValue),
              CurrentValue);
        if VarIsStr(CurrentValue) and (loPartialKey in Options) then
          CurrentValue := Copy(CurrentValue, 1, Length(LocalKeyValues));
      end;
      Result := SameValue(CurrentValue, LocalKeyValues);
    end);
  if Enumerable.Any then
    Index := InternalDataList.IndexOf(Enumerable.First);
end;

procedure TListDataSet<T>._OnLookupValue(
  Sender: TCustomVirtualDataset;
  const KeyFields: string;
  const KeyValues: Variant;
  const ResultFields: string;
  var Value: Variant);
var
  Index: Integer;
  VarArray: TArray<variant>;
  Tokenizer: IShared<TStringList>;
  TokenIndex: Integer;
  FieldValue: Variant;
  Item: T;
begin
  Value := null;
  _OnLocate(Sender, KeyFields, KeyValues, [], Index);
  if Index = -1 then
    Exit;

  Item := InternalDataList[Index];

  Tokenizer := Shared.Make(TStringList.Create);
  Tokenizer.Text := StringReplace(ResultFields, ';', #13#10, [rfReplaceAll]);
  SetLength(VarArray, Tokenizer.Count);
  for TokenIndex := 0 to Tokenizer.Count - 1 do
    if GetEntityFieldValue(TValue.From<T>(Item), '', Tokenizer[TokenIndex], FieldValue) then
      VarArray[TokenIndex] := FieldValue;

  if Tokenizer.Count = 1 then
    Value := VarArray[0]
  else
    Value := VarArray;
end;

procedure TListDataSet<T>._OnPostData(
  Sender: TCustomVirtualDataset;
  const Index: Integer);
var
  Entity: TValue;
begin
  case Sender.State of
    dsEdit: begin
      RecordToEntity(Index);
      FilterDataset;
    end;
    dsInsert: begin
      Entity := FEntityFactoryFunc(TypeInfo(T));
      RecordToEntity(Entity, '');
      FTraversedTypeInfoMap.Clear;
      //Append
      if Index = -1 then
        FOriginalDataList.Add(Entity.AsType<T>)
      //Insert
      else
        FOriginalDataList.Insert(Index, Entity.AsType<T>);
      FilterDataset;
    end;
  end;
end;

procedure TListDataSet<T>.SetDataList(const Value: IList<T>);
var
  Field: TField;
  Attribute: TDatasetFieldAttribute;
begin
  Close;
  Fields.Clear;

  if Assigned(FOriginalDataList) then
    FOriginalDataList.OnChanged.Remove(ListChangedEvent);

  FOriginalDataList := Value;

  if Assigned(FOriginalDataList) then
  begin
    FOriginalDataList.OnChanged.Add(ListChangedEvent);
    InternalInitFieldDefsObjectClass(TypeInfo(T), '');
    CreateFields;
  end;

  //Inject the FieldAttributes. if field attributes are defined then show only defined fields.
  if FDatasetFieldAttributes.Count = 0 then
    Exit;

  for Field in Fields do
    Field.Visible := False;

  for Field in Fields do
    if FDatasetFieldAttributes.TryGetAttribute(Field.FieldName, Attribute) then
    begin
      Field.DisplayWidth := Attribute.Width;
      Field.DisplayLabel := Attribute.Title;
      Field.ReadOnly := Attribute.ReadOnly;
      Field.EditMask := Attribute.EditMask;
      Field.Visible := Attribute.Visible;
      if Attribute.Precision = 0 then
        Continue;
      if Field is TCurrencyField then
        (Field as TCurrencyField).Precision := Attribute.Precision
      else if Field is TFloatField then
        (Field as TFloatField).Precision := Attribute.Precision
      else if Field is TSingleField then
        (Field as TSingleField).Precision := Attribute.Precision
      else if Field is TSingleField then
        (Field as TSingleField).Precision := Attribute.Precision
      else if Field is TExtendedField then
        (Field as TExtendedField).Precision := Attribute.Precision
      else if Field is TBCDField then
        (Field as TBCDField).Precision := Attribute.Precision
      else if Field is TFMTBCDField then
        (Field as TFMTBCDField).Precision := Attribute.Precision;
      if Attribute.Precision <> 0 then
        Field.OnGetText := OnFieldGetText;
    end;
end;

procedure TListDataSet<T>.SetFiltered(Value: Boolean);
begin
  inherited;
  FilterDataSet;
end;

procedure TListDataSet<T>.SetOnCheckItemVisibility(const Value: Predicate<T>);
begin
  FOnCheckItemVisibility := Value;
  FilterDataSet;
end;

function TListDataSet<T>.GetIsGetterName(const Name: string): boolean;
begin
  Result := Name.ToUpper.StartsWith(GETTER_PREFIX, true);
end;

function TListDataSet<T>.GetMappedName(const Name: string): string;
begin
  Result := Name.ToUpper;

  if GetIsGetterName(Result) then
    Result := Copy(Result, Length(GETTER_PREFIX) + 1, Length(Result));
end;

function TListDataSet<T>.TryGetGetterMethodInfo(
  const RttiMeth: TRttiMethod;
  out MethodInfo: TMethodInfo): Boolean;
begin
  if not ((RttiMeth.Visibility in [mvPublic, mvPublished]) and
          (RttiMeth.MethodKind = mkFunction) and
          (Length(RttiMeth.GetParameters) = 0)) then
    Exit(False);

  MethodInfo.IsList := Assigned(RttiMeth.ReturnType) and RttiMeth.ReturnType.QualifiedName.ToUpper.Contains('ILIST<');

  MethodInfo.FieldName := GetMappedName(RttiMeth.Name);

  if MethodInfo.FieldName.IsEmpty then
    Exit(False);

  MethodInfo.TypeKind := RttiMeth.ReturnType.TypeKind;
  MethodInfo.VariableTypeName := RttiMeth.ReturnType.QualifiedName;
  MethodInfo.Handle := RttiMeth.ReturnType.Handle;
  Result := True;
end;

function TListDataSet<T>.TryGetSetterMethodInfo(
  const RttiMeth: TRttiMethod;
  out MethodInfo: TMethodInfo): Boolean;
begin
  if not ((RttiMeth.Visibility in [mvPublic, mvPublished]) and
          (RttiMeth.MethodKind = mkProcedure) and
          (Length(RttiMeth.GetParameters) = 1)) then
    Exit(False);


  Result := RttiMeth.Name.ToUpper.StartsWith(SETTER_PREFIX);
  if not Result then
    Exit;

  MethodInfo.FieldName := RttiMeth.Name.Remove(0, Length(SETTER_PREFIX));

  if MethodInfo.FieldName.IsEmpty then
    Exit(False);

  MethodInfo.TypeKind := RttiMeth.GetParameters[0].ParamType.TypeKind;
  MethodInfo.VariableTypeName := RttiMeth.GetParameters[0].ParamType.QualifiedName;
  MethodInfo.Handle := RttiMeth.GetParameters[0].ParamType.Handle;

  MethodInfo.IsList := Assigned(RttiMeth.GetParameters[0].ParamType) and RttiMeth.GetParameters[0].ParamType.QualifiedName.ToUpper.Contains('ILIST<');
end;

{ TListDataSetOfObservable<T> }

constructor TListDataSetOfObservable<T>.Create(
  Owner: TComponent;
  const EntityFactoryFunc: TFunc<PTypeInfo, TValue>);
begin
  inherited Create(Owner, EntityFactoryFunc);
  FListFactory :=
    function: IList<T>
    begin
      Result := TCollections.GetListOfDeepObservable<T>;
    end;
end;

end.

