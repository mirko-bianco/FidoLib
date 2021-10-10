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

unit Fido.Db.VirtualDataset;
{$I Jedi.inc}
{$R-,T-,H+,X+}
interface

uses
  Classes,
  DB,
  SysUtils,
  DbConsts,
  dbcommon,
  Variants,
  FMTBcd,
  Math,
  WideStrUtils,

  Spring,
  Spring.Data.ActiveX,

  Fido.Exceptions;

const
  BFNA = TBookmarkFlag(Ord( High(TBookmarkFlag)) + 1);
  DERECORDCOUNTCHANGED = TDataEvent(Ord( High(TDataEvent)) + 1);

resourcestring
  SUnsupportedFieldType = 'Unsupported field type (%s) in field %s.';
  SPersistentFieldsRequired = 'Virtual dataset can only be used with persistent fields.';
  SIndexOutOfRange = 'Index out of range';
  SDataListNotAssigned = 'DataList property not set, cannot open Dataset.';
  SNoPropertyInformation = 'List does not implement IPropertyInformation interface.';
  SInvalidPropertyType = 'Property type not supported for property ''%s''';
  SNoPropertyValueInterface = 'List item does not implement IPropertyValue interface.';

type
  TCustomVirtualDataset = class;
  TVirtualDataset = class;

  EFidoVirtualDatasetError = class(EFidoException);

  PVariantList = ^TVariantList;
  TVariantList = array [0 .. 0] of OleVariant;

  TDeleteRecordEvent = procedure(Sender: TCustomVirtualDataset; const Index: Integer) of object;
  TGetRecordCountEvent = procedure(Sender: TCustomVirtualDataset; var Count: Integer) of object;
  TGetFieldValueEvent = procedure(Sender: TCustomVirtualDataset; const Field: TField; const Index: Integer; var Value: Variant) of object;
  TPostDataEvent = procedure(Sender: TCustomVirtualDataset; const Index: Integer) of object;
  TLocateEvent = procedure(Sender: TCustomVirtualDataset; const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions; var Index: Integer) of object;
  TLookupValueEvent = procedure(Sender: TCustomVirtualDataset; const KeyFields: string; const KeyValues: Variant; const ResultFields: string; var Value: Variant) of object;
  PArrayRecInfo = ^TArrayRecInfo;
  TArrayRecInfo = record
    Index: Integer;
    BookmarkFlag: TBookmarkFlag;
  end;

  { TADBlobStream }

  TADBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TCustomVirtualDataset;
    FBuffer: TRecordBuffer;
    FFieldNo: Integer;
    FModified: Boolean;
    FData: Variant;
    FFieldData: Variant;
  protected
    procedure ReadBlobData;
    {$IFDEF DELPHIX_ALEXANDRIA_UP}
    function Realloc(var NewCapacity: Nativeint): Pointer; override;
    {$ELSE}
    function Realloc(var NewCapacity: Longint): Pointer; override;
    {$ENDIF}
  public
    constructor Create(const Field: TBlobField; const Mode: TBlobStreamMode);
    destructor Destroy; override;

    function Write(
      const Buffer;
      Count: Longint): Longint; override;
    procedure Truncate;
  end;

  TVirtualMasterDataLink = class(TMasterDataLink)
  protected
    procedure ActiveChanged; override;
  end;

  TCustomVirtualDataset = class(TDataSet, IUnknown)
  protected
    FInternalOpen: Boolean;
    FCurrent: Integer;
    FFilterBuffer: TRecordBuffer;
    FReadOnly: Boolean;
    FRecBufSize: Integer;
    FMasterDataLink: TVirtualMasterDataLink;
    FModifiedFields: TList;
    FOldValueBuffer: TRecordBuffer;
    FOnDeleteRecord: TDeleteRecordEvent;
    FOnGetFieldValue: TGetFieldValueEvent;
    FOnGetRecordCount: TGetRecordCountEvent;
    FOnPostData: TPostDataEvent;
    FOnLocate: TLocateEvent;
    FOnLookupValue: TLookupValueEvent;
    Reserved: Pointer;

    procedure DoDeleteRecord(const Index: Integer); virtual;
    procedure DoGetFieldValue(const Field: TField; const Index: Integer; var Value: Variant); virtual;
    procedure DoPostData(const Index: Integer); virtual;
    function InternalGetRecord(const Buffer: TRecordBuffer; const GetMode: TGetMode; const DoCheck: Boolean): TGetResult; virtual;
    function GetMasterSource: TDataSource;
    function GetTopIndex: Integer;
    function GetTopRecNo: Integer;
    function GetIndex: Integer;
    procedure MasterChanged(Sender: TObject); virtual;
    procedure MasterDisabled(Sender: TObject); virtual;
    procedure SetIndex(const Value: Integer);
    procedure SetTopIndex(const Value: Integer);
    procedure SetMasterSource(const Value: TDataSource);
    function GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean;
    function GetCanModify: Boolean; override;
    function GetRecNo: Integer; override;
    function GetRecordCount: Integer; override;
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    procedure DoOnNewRecord; override;
    procedure InternalEdit; override;
    procedure SetRecNo(Value: Integer); override;
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalClose; override;
    procedure InternalCreateFields; virtual;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); override;
    procedure InternalSetFieldData(const Field: TField; const Buffer: Pointer; const NativeFormat: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData): Integer; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean; override;

    property Current: Integer read FCurrent;
    property Index: Integer read GetIndex write SetIndex;
    property MasterDataLink: TVirtualMasterDataLink read FMasterDataLink;
    property MasterSource: TDataSource read GetMasterSource write SetMasterSource;
    property ModifiedFields: TList read FModifiedFields;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property TopRecNo: Integer read GetTopRecNo;
    property OnDeleteRecord : TDeleteRecordEvent read FOnDeleteRecord write FOnDeleteRecord;
    property OnGetFieldValue: TGetFieldValueEvent read FOnGetFieldValue write FOnGetFieldValue;
    property OnGetRecordCount : TGetRecordCountEvent read FOnGetRecordCount write FOnGetRecordCount;
    property OnLocate: TLocateEvent read FOnLocate write FOnLocate;
    property OnLookupValue : TLookupValueEvent read FOnLookupValue write FOnLookupValue;
    property OnPostData: TPostDataEvent read FOnPostData write FOnPostData;
  end;

  TVirtualDataset = class(TCustomVirtualDataset)
  published
    property Active;
    property Filtered;
    property ReadOnly;
    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;
    property MasterSource;
    property OnCalcFields;
    property OnDeleteError;
    property OnDeleteRecord;
    property OnEditError;
    property OnFilterRecord;
    property OnGetFieldValue;
    property OnGetRecordCount;
    property OnNewRecord;
    property OnLookupValue;
    property OnLocate;
    property OnPostData;
    property OnPostError;
  end;

procedure VirtualDatasetError(const Message: string; Dataset: TCustomVirtualDataset = nil);
procedure VirtualDatasetErrorFmt(const Message: string; const Args: array of const; Dataset: TCustomVirtualDataset = nil);

implementation

function FieldListCheckSum(Dataset: TDataSet): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Dataset.Fields.Count - 1 do
    Result := Result + (Integer(Dataset.Fields[I]) shr (I mod 16));
end;

procedure VirtualDatasetError(
  const Message: string;
  Dataset: TCustomVirtualDataset = nil);
begin
  if Assigned(Dataset) then
    raise EFidoVirtualDatasetError.Create(Format('%s: %s', [Dataset.Name, Message]))
  else
    raise EFidoVirtualDatasetError.Create(Message);
end;

procedure VirtualDatasetErrorFmt(
  const Message: string;
  const Args: array of const;
  Dataset: TCustomVirtualDataset = nil);
begin
  VirtualDatasetError(Format(Message, Args), Dataset);
end;

{ TADBlobStream }

constructor TADBlobStream.Create(
  const Field: TBlobField;
  const Mode: TBlobStreamMode);
begin
  Guard.CheckNotNull(Field, 'Field');
  FField := Field;
  FFieldNo := FField.FieldNo - 1;
  FDataSet := FField.Dataset as TCustomVirtualDataset;
  FFieldData := Null;
  FData := Null;
  if not FDataSet.GetActiveRecBuf(FBuffer) then
    Exit;
  if Mode <> bmRead then
  begin
    if FField.ReadOnly then
      DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName], FDataSet);
    if not(FDataSet.State in [dsEdit, dsInsert]) then
      DatabaseError(SNotEditing, FDataSet);
  end;
  if Mode = bmWrite then
    Truncate
  else
    ReadBlobData;
end;

destructor TADBlobStream.Destroy;
begin
  if FModified then
    try
      FDataSet.SetFieldData(FField, @FData);
      FField.Modified := True;
      FDataSet.DataEvent(deFieldChange, Longint(FField));
    except
      ApplicationHandleException(Self);
    end;
  inherited Destroy;
end;

procedure TADBlobStream.ReadBlobData;
var
  buffer: TBytes;
  recBuf: TRecordBuffer;
begin
  fFieldData := Null;
  if fDataSet.GetFieldData(fField, buffer, True) then
    if fDataSet.GetActiveRecBuf(recBuf) then
      fFieldData := PVariantList(recBuf + SizeOf(TArrayRecInfo))[fField.Index];
  if not VarIsNull(fFieldData) then
  begin
    if VarType(fFieldData) = varOleStr then
    begin
      if fField.BlobType = ftWideMemo then
        Size := Length(WideString(fFieldData)) * SizeOf(WideChar)
      else
      begin
{$IFNDEF NEXTGEN}
        fFieldData := AnsiString(fFieldData);
{$ELSE}
        fFieldData := VarToStr(fFieldData);
{$ENDIF}
        Size := Length(fFieldData);
      end;
    end
    else
      Size := VarArrayLength(fFieldData, 1);
    fFieldData := Null;
  end;
end;

{$IFDEF DELPHIX_ALEXANDRIA_UP}
function TADBlobStream.Realloc(var NewCapacity: Nativeint): Pointer;
{$ELSE}
function TADBlobStream.Realloc(var NewCapacity: Longint): Pointer;
{$ENDIF}
  procedure VarAlloc(var V: Variant; Field: TFieldType);
  var
    W: Widestring;
    S: Ansistring;
  begin
    if Field = ftMemo then
    begin
      if not VarIsNull(V) then
        S := Ansistring(V);
      SetLength(S, NewCapacity);
      V := S;
    end
    else if Field = ftWideMemo then
    begin
      if not VarIsNull(V) then
        W := Widestring(V);
      SetLength(W, NewCapacity div 2);
      V := W;
    end
    else
    begin
      if VarIsClear(V) or VarIsNull(V) then
        V := VarArrayCreate([0, NewCapacity - 1], varByte)
      else
        VarArrayRedim(V, NewCapacity - 1);
    end;
  end;

begin
  Result := Memory;
  if NewCapacity <> Capacity then
  begin
    if VarIsArray(FData) then
      VarArrayUnlock(FData);
    if NewCapacity = 0 then
    begin
      FData := Null;
      Result := nil;
    end
    else
    begin
      if VarIsNull(FFieldData) then
        VarAlloc(FData, FField.DataType)
      else
        FData := FFieldData;
      if VarIsArray(FData) then
        Result := VarArrayLock(FData)
      else
        Result := TVarData(FData).Vstring;
    end;
  end;
end;

function TADBlobStream.Write(
  const Buffer;
  Count: Longint): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;

procedure TADBlobStream.Truncate;
begin
  Clear;
  FModified := True;
end;

// =---------------------------------------------------------------------------=
// TEzVirtualMasterDataLink
// =---------------------------------------------------------------------------=
procedure TVirtualMasterDataLink.ActiveChanged;
begin
  if Dataset = nil then
    Exit;

  // Fake a field.
  if Fields.Count = 0 then
    Fields.Add(TField.Create(Dataset));

  if Dataset.Active and not(csDestroying in Dataset.ComponentState) then
    if Active then
    begin
      if Assigned(OnMasterChange) then
        OnMasterChange(Self);
    end
    else if Assigned(OnMasterDisable) then
      OnMasterDisable(Self);
end;

// =---------------------------------------------------------------------------=
// TCustomVirtualDataset
// =---------------------------------------------------------------------------=
constructor TCustomVirtualDataset.Create(AOwner: TComponent);
begin
  inherited;
  FInternalOpen := False;
  FReadOnly := False;
  FModifiedFields := TList.Create;
  FMasterDataLink := TVirtualMasterDataLink.Create(Self);
  MasterDataLink.OnMasterChange := MasterChanged;
  MasterDataLink.OnMasterDisable := MasterDisabled;
end;

destructor TCustomVirtualDataset.Destroy;
begin
  inherited;
  FModifiedFields.Free;
  FMasterDataLink.Free;
end;

function TCustomVirtualDataset.AllocRecordBuffer: TRecordBuffer;
begin
  if not(csDestroying in ComponentState) then
  begin
    Result := AllocMem(FRecBufSize);
    Initialize(PVariantList(Result + sizeof(TArrayRecInfo))^, Fields.Count);
  end
  else
    Result := nil;
end;

function TCustomVirtualDataset.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  if Assigned(Bookmark) and (PInteger(Bookmark)^ >= 0) and
    (PInteger(Bookmark)^ < RecordCount) then
    Result := True
  else
    Result := False;
end;

function TCustomVirtualDataset.CompareBookmarks(
  Bookmark1: TBookmark;
  Bookmark2: TBookmark): Integer;
const
  RetCodes: array [Boolean, Boolean] of ShortInt = ((2, -1), (1, 0));

begin
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then
  begin
    if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ then
      Result := -1
    else if PInteger(Bookmark1)^ > PInteger(Bookmark2)^ then
      Result := 1
    else
      Result := 0;
  end;
end;

function TCustomVirtualDataset.CreateBlobStream(
  Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  Result := TADBlobStream.Create(Field as TBlobField, Mode);
end;

procedure TCustomVirtualDataset.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  case Event of
    deLayoutChange:
      if Active and Assigned(Reserved) and
        (FieldListCheckSum(Self) <> Integer(Reserved)) then
        Reserved := nil;
  end;
  inherited;
end;

procedure TCustomVirtualDataset.DoDeleteRecord(const Index: Integer);
begin
  if Assigned(FOnDeleteRecord) then
    FOnDeleteRecord(Self, Index);
end;

procedure TCustomVirtualDataset.DoGetFieldValue(
  const Field: TField;
  const Index: Integer;
  var Value: Variant);
begin
  if Assigned(FOnGetFieldValue) then
    FOnGetFieldValue(Self, Field, Index, Value);
end;

procedure TCustomVirtualDataset.DoOnNewRecord;
begin
  FModifiedFields.Clear;

  if FOldValueBuffer = nil then
    FOldValueBuffer := AllocRecordBuffer
  else
    Finalize(PVariantList(FOldValueBuffer + sizeof(TArrayRecInfo))^,
      Fields.Count);

  {$WARNINGS OFF}
  InitRecord(FOldValueBuffer);
  {$WARNINGS ON}

  inherited DoOnNewRecord;
end;

procedure TCustomVirtualDataset.DoPostData(const Index: Integer);
begin
  if Assigned(FOnPostData) then
    FOnPostData(Self, Index);
end;

procedure TCustomVirtualDataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  Finalize(PVariantList(Buffer + sizeof(TArrayRecInfo))^, Fields.Count);
  FreeMem(Buffer);
end;

function TCustomVirtualDataset.GetActiveRecBuf(var RecBuf: TRecordBuffer)
  : Boolean;
begin
  RecBuf := nil;
  case State of
    dsBlockRead, dsBrowse:
      if IsEmpty then
        RecBuf := nil
      else
        RecBuf := TRecordBuffer(ActiveBuffer);

    dsEdit, dsInsert, dsNewValue:
      RecBuf := TRecordBuffer(ActiveBuffer);

    dsCalcFields, dsInternalCalc:
      RecBuf := TRecordBuffer(CalcBuffer);

    dsFilter:
      RecBuf := TRecordBuffer(FFilterBuffer);
  end;
  Result := RecBuf <> nil;
end;

function TCustomVirtualDataset.GetBlobFieldData(
  FieldNo: Integer;
  var Buffer: TBlobByteData): Integer;
begin
  Result := inherited GetBlobFieldData(FieldNo, Buffer);
end;

procedure TCustomVirtualDataset.GetBookmarkData(
  Buffer: TRecordBuffer;
  Data: Pointer);
begin
  PInteger(Data)^ := PArrayRecInfo(Buffer)^.Index;
end;

function TCustomVirtualDataset.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PArrayRecInfo(Buffer)^.BookmarkFlag;
end;

function TCustomVirtualDataset.GetCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

function TCustomVirtualDataset.GetFieldData(
  Field: TField;
  var Buffer: TValueBuffer): Boolean;
var
  recBuf: TRecordBuffer;
  Data: Variant;
  {$IFDEF DELPHIXE3_UP}
  procedure CurrToBuffer(const C: Currency);
  var
    buff: TValueBuffer;
  begin
    SetLength(buff, SizeOf(Currency));
    TDBBitConverter.UnsafeFrom<Currency>(C, buff);
    DataConvert(Field, buff, Buffer, True)
  end;
  {$ELSE}
  procedure CurrToBuffer(const C: Currency);
  begin
    DataConvert(Field, @C, Buffer, True)
  end;
  {$ENDIF}
  {$IFDEF DELPHIXE3_UP}
  procedure VarToBuffer;
  var
    TempBuff: TValueBuffer;
    PData: Pointer;
  begin
    case Field.DataType of
      ftGuid, ftFixedChar, ftString:
      begin
        PAnsiChar(Buffer)[Field.Size] := #0;
        TempBuff := TEncoding.Default.GetBytes(string(tagVariant(Data).bStrVal));
        Move(TempBuff[0], Buffer[0], Length(TempBuff));
        PAnsiChar(Buffer)[Min(Field.Size, Length(TempBuff))] := #0;
      end;
      ftFixedWideChar, ftWideString:
      begin
        if tagVariant(Data).vt = VT_BSTR then
          TempBuff := TEncoding.Unicode.GetBytes(tagVariant(Data).bstrVal)
        else
          TempBuff := TEncoding.Unicode.GetBytes(string(Data));
        SetLength(TempBuff, Length(TempBuff) + SizeOf(Char));
        TempBuff[Length(TempBuff) - 2] := 0;
        TempBuff[Length(TempBuff) - 1] := 0;
        Move(TempBuff[0], Buffer[0], Length(TempBuff));
      end;
      ftShortint:
        if tagVariant(Data).vt = VT_UI1 then
          TDBBitConverter.UnsafeFrom<ShortInt>(ShortInt(tagVariant(Data).cVal), Buffer)
        else
          TDBBitConverter.UnsafeFrom<ShortInt>(ShortInt(tagVariant(Data).bVal), Buffer);
      ftByte:
        if tagVariant(Data).vt = VT_UI1 then
          TDBBitConverter.UnsafeFrom<Byte>(Byte(tagVariant(Data).cVal), Buffer)
        else
          TDBBitConverter.UnsafeFrom<Byte>(tagVariant(Data).bVal, Buffer);
      ftSmallint:
        if tagVariant(Data).vt = VT_UI1 then
          TDBBitConverter.UnsafeFrom<SmallInt>(Byte(tagVariant(Data).cVal), Buffer)
        else
          TDBBitConverter.UnsafeFrom<SmallInt>(tagVariant(Data).iVal, Buffer);
      ftWord:
        if tagVariant(Data).vt = VT_UI1 then
          TDBBitConverter.UnsafeFrom<Word>(tagVariant(Data).bVal, Buffer)
        else
          TDBBitConverter.UnsafeFrom<Word>(tagVariant(Data).uiVal, Buffer);
      ftAutoInc, ftInteger:
        TDBBitConverter.UnsafeFrom<Integer>(Data, Buffer);
      ftLongWord:
        TDBBitConverter.UnsafeFrom<Cardinal>(Data, Buffer);
      ftFloat, ftCurrency:
        if tagVariant(Data).vt = VT_R8 then
          TDBBitConverter.UnsafeFrom<Double>(tagVariant(Data).dblVal, Buffer)
        else
          TDBBitConverter.UnsafeFrom<Double>(Data, Buffer);
      ftExtended:
        if tagVariant(Data).vt = VT_R8 then
          TDBBitConverter.UnsafeFrom<Extended>(tagVariant(Data).dblVal, Buffer)
        else
          TDBBitConverter.UnsafeFrom<Extended>(Data, Buffer);
      ftFMTBCD:
        TDBBitConverter.UnsafeFrom<TBcd>(VarToBcd(Data), Buffer);
      ftBCD:
        if tagVariant(Data).vt = VT_CY then
          CurrToBuffer(tagVariant(Data).cyVal)
        else
          CurrToBuffer(Data);
      ftBoolean:
        TDBBitConverter.UnsafeFrom<WordBool>(tagVariant(Data).vbool, Buffer);
      ftDate, ftTime, ftDateTime:
        begin
          SetLength(TempBuff, SizeOf(Double));
          TDBBitConverter.UnsafeFrom<Double>(data, TempBuff);
          DataConvert(Field, TempBuff, Buffer, True);
        end;
      ftBytes, ftVarBytes:
        begin
          PData := VarArrayLock(Data);
          try
            DataConvert(Field, BytesOf(PData, VarArrayLength(Data, 1)), Buffer, True);
          finally
            VarArrayUnlock(Data);
          end;
        end;
      ftInterface:
      begin
        TempBuff := BytesOf(@Data, SizeOf(IUnknown));
        Move(TempBuff[0], Buffer[0], SizeOf(IUnknown));
      end;
      ftIDispatch:
      begin
        TempBuff := BytesOf(@Data, SizeOf(IDispatch));
        Move(TempBuff[0], Buffer[0], SizeOf(IDispatch));
      end;
      ftLargeInt:
      begin
        if PDecimal(@Data).sign > 0 then
          TDBBitConverter.UnsafeFrom<Int64>(-1 * PDecimal(@Data).Lo64, Buffer)
        else
          TDBBitConverter.UnsafeFrom<Int64>(PDecimal(@Data).Lo64, Buffer);
      end;
      ftBlob..ftTypedBinary, ftVariant, ftWideMemo: TDBBitConverter.UnsafeFromVariant(Data, Buffer);
    else
      DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[Field.DataType],
        Field.DisplayName]);
    end;
  end;
  {$ELSE}
  procedure VarToBuffer;
  var
    TempBuff: TArray<Byte>;
  begin
    case Field.DataType of
      ftGuid, ftFixedChar, ftString:
      begin
        PAnsiChar(Buffer)[Field.Size] := #0;
        TempBuff := TEncoding.Default.GetBytes(string(tagVariant(Data).bStrVal));
        Move(TempBuff[0], PByte(Buffer)[0], Length(TempBuff));
        PAnsiChar(Buffer)[Min(Field.Size, Length(TempBuff))] := #0;
      end;
      ftFixedWideChar, ftWideString:
      begin
        if tagVariant(Data).vt = VT_BSTR then
          TempBuff := TEncoding.Unicode.GetBytes(tagVariant(Data).bstrVal)
        else
          TempBuff := TEncoding.Unicode.GetBytes(string(Data));
        SetLength(TempBuff, Length(TempBuff) + SizeOf(Char));
        TempBuff[Length(TempBuff) - 2] := 0;
        TempBuff[Length(TempBuff) - 1] := 0;
        Move(TempBuff[0], PByte(Buffer)[0], Length(TempBuff));
      end;
      ftShortint:
        if tagVariant(Data).vt = VT_UI1 then
          ShortInt(Buffer^) := ShortInt(tagVariant(Data).cVal)
        else
          ShortInt(Buffer^) := ShortInt(tagVariant(Data).bVal);
      ftByte:
        if tagVariant(Data).vt = VT_UI1 then
          Byte(Buffer^) := Byte(tagVariant(Data).cVal)
        else
          Byte(Buffer^) := tagVariant(Data).bVal;
      ftSmallint:
        if tagVariant(Data).vt = VT_UI1 then
          SmallInt(Buffer^) := Byte(tagVariant(Data).cVal)
        else
          SmallInt(Buffer^) := tagVariant(Data).iVal;
      ftWord:
        if tagVariant(Data).vt = VT_UI1 then
          Word(Buffer^) := tagVariant(Data).bVal
        else
          Word(Buffer^) := tagVariant(Data).uiVal;
      ftAutoInc, ftInteger:
        Integer(Buffer^) := Data;
      ftLongWord:
        Cardinal(Buffer^) := tagVariant(Data).uintVal;
      ftFloat, ftCurrency:
        if tagVariant(Data).vt = VT_R8 then
          Double(Buffer^) := tagVariant(Data).dblVal
        else
          Double(Buffer^) := Data;
      ftFMTBCD:
        TBcd(Buffer^) := VarToBcd(Data);
      ftBCD:
        if tagVariant(Data).vt = VT_CY then
          CurrToBuffer(tagVariant(Data).cyVal)
        else
          CurrToBuffer(Data);
      ftBoolean:
      begin
        VarAsType(Data, VT_BOOL);
        WordBool(Buffer^) := tagVariant(Data).vbool;
      end;
      ftDate, ftTime, ftDateTime:
      begin
        VarAsType(Data, VT_DATE);
          DataConvert(Field, @date, Buffer, True);
      end;
      ftBytes, ftVarBytes:
          DataConvert(Field, @Data, Buffer, True);
      ftInterface:
        IUnknown(Buffer^) := Data;
      ftIDispatch:
        IDispatch(Buffer^) := Data;
      ftLargeInt:
        if PDecimal(@Data).sign > 0 then
          LargeInt(Buffer^) := -1 * PDecimal(@Data).Lo64
        else
          LargeInt(Buffer^) := PDecimal(@Data).Lo64;
      ftBlob .. ftTypedBinary, ftVariant, ftWideMemo:
        OleVariant(Buffer^) := Data;
    else
      DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[Field.DataType],
        Field.DisplayName]);
    end;
  end;
  {$ENDIF}
  procedure RefreshBuffers;
  begin
    Reserved := Pointer(FieldListCheckSum(Self));
    UpdateCursorPos;
    Resync([]);
  end;
  function DataToInt64: Int64;
  begin
    if PDecimal(@Data).sign > 0 then
      Result := -1 * PDecimal(@Data).Lo64
    else
      Result := PDecimal(@Data).Lo64;
  end;
begin
  if not Assigned(Reserved) then
    RefreshBuffers;
  Result := GetActiveRecBuf(recBuf);
  if not Result then
    Exit;
  Data := PVariantList(recBuf + SizeOf(TArrayRecInfo))[Field.Index];
  if VarIsEmpty(Data) then
  begin
    DoGetFieldValue(Field, PArrayRecInfo(recBuf).Index, Data);
    if VarIsEmpty(Data) then
      Data := Null;
    if VarType(Data) = varInt64 then
      PVariantList(recBuf + SizeOf(TArrayRecInfo))[Field.Index] := DataToInt64
    else
      PVariantList(recBuf + SizeOf(TArrayRecInfo))[Field.Index] := Data;
  end;
  Result := not VarIsNull(Data);
  if Result and (Buffer <> nil) then
    VarToBuffer;
end;

function TCustomVirtualDataset.GetIndex: Integer;
begin
  Result := RecNo;
  if Result > -1 then
    dec(Result);
end;

function TCustomVirtualDataset.GetMasterSource: TDataSource;
begin
  Result := MasterDataLink.DataSource;
end;

function TCustomVirtualDataset.GetRecNo: Integer;
var
  RecBuf: TRecordBuffer;
begin
  CheckActive;
  Result := -1;
  if GetActiveRecBuf(RecBuf) and
    (PArrayRecInfo(RecBuf)^.BookmarkFlag = bfCurrent) then
    Result := PArrayRecInfo(RecBuf)^.Index + 1;
end;

function TCustomVirtualDataset.GetRecord(
  Buffer: TRecordBuffer;
  GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  Accept: Boolean;
  SaveState: TDataSetState;
begin
  if Filtered and Assigned(OnFilterRecord) then
  begin
    FFilterBuffer := Buffer;
    SaveState := SetTempState(dsFilter);
    try
      Accept := True;
      repeat
        Result := InternalGetRecord(Buffer, GetMode, DoCheck);
        if Result = grOK then
        begin
          OnFilterRecord(Self, Accept);
          if not Accept and (GetMode = gmCurrent) then
            Result := grError;
        end;
      until Accept or (Result <> grOK);
    except
      Result := grError;
    end;
    RestoreState(SaveState);
  end
  else
    Result := InternalGetRecord(Buffer, GetMode, DoCheck)
end;

function TCustomVirtualDataset.InternalGetRecord(
  const Buffer: TRecordBuffer;
  const GetMode: TGetMode;
  const DoCheck: Boolean): TGetResult;
var
  iRecCount: Integer;
begin
  try
    Result := grOK;
    case GetMode of
      gmNext:
        begin
          iRecCount := RecordCount;
          if FCurrent < iRecCount then
            inc(FCurrent);
          if FCurrent >= iRecCount then
            Result := grEOF;
        end;
      gmPrior:
        begin
          if FCurrent <= 0 then
            FCurrent := -1

          else
          begin
            iRecCount := RecordCount;
            FCurrent := Min(FCurrent - 1, iRecCount - 1);
          end;

          if FCurrent < 0 then
            Result := grBOF;
        end;

      gmCurrent:
        begin
          iRecCount := RecordCount;
          if FCurrent < 0 then
            Result := grBOF
          else if FCurrent >= iRecCount then
            Result := grEOF;
        end;
    end;

    if Result = grOK then
    begin
      with PArrayRecInfo(Buffer)^ do
      begin
        Index := FCurrent;
        BookmarkFlag := bfCurrent;
      end;
      Finalize(PVariantList(Buffer + sizeof(TArrayRecInfo))^, Fields.Count);
      {$WARNINGS OFF}
      GetCalcFields(Buffer);
      {$WARNINGS ON}
    end;

  except
    if DoCheck then
      raise ;
    Result := grError;
  end;
end;

function TCustomVirtualDataset.GetRecordCount: Integer;
begin
  Result := -1;
  if Assigned(FOnGetRecordCount) then
    FOnGetRecordCount(Self, Result);
end;

function TCustomVirtualDataset.GetRecordSize: Word;
begin
  Result := sizeof(TArrayRecInfo);
end;

function TCustomVirtualDataset.GetTopIndex: Integer;
begin
  if BufferCount = 0 then
    Result := -1
  else
    Result := PArrayRecInfo(Buffers[0])^.Index;
end;

function TCustomVirtualDataset.GetTopRecNo: Integer;
begin
  Result := TopIndex + 1;
end;

procedure TCustomVirtualDataset.InternalAddRecord
  (Buffer: Pointer; Append: Boolean);
begin
end;

procedure TCustomVirtualDataset.InternalClose;
begin
  FInternalOpen := False;
  BindFields(False);
  FieldDefs.Updated := False;
  if FOldValueBuffer <> nil then
    try
      Finalize(PVariantList(FOldValueBuffer + sizeof(TArrayRecInfo))^,
        Fields.Count);
      FreeMem(FOldValueBuffer);
    finally
      FOldValueBuffer := nil;
    end;
end;

procedure TCustomVirtualDataset.InternalCreateFields;
begin
  //
  // TCustomVirtualDataset can only handle persistent fields
  //
  {$WARNINGS OFF}
  if DefaultFields then
    VirtualDatasetError(SPersistentFieldsRequired, Self);
  {$WARNINGS ON}
end;

procedure TCustomVirtualDataset.InternalDelete;
var
  RecBuf: TRecordBuffer;

begin
  GetActiveRecBuf(RecBuf);
  DoDeleteRecord(PArrayRecInfo(RecBuf)^.Index);
end;

procedure TCustomVirtualDataset.InternalEdit;
begin
  FModifiedFields.Clear;

  if FOldValueBuffer = nil then
    FOldValueBuffer := AllocRecordBuffer
  else
    Finalize(PVariantList(FOldValueBuffer + sizeof(TArrayRecInfo))^,
      Fields.Count);
end;

procedure TCustomVirtualDataset.InternalFirst;
begin
  FCurrent := -1;
end;

procedure TCustomVirtualDataset.InternalGotoBookmark(Bookmark: Pointer);
begin
  FCurrent := PInteger(Bookmark)^;
end;

procedure TCustomVirtualDataset.InternalHandleException;
begin
  //Application.HandleException(Self);
end;

procedure TCustomVirtualDataset.InternalInitFieldDefs;
var
  FieldDef: TFieldDef;

  procedure InitFieldDefsFromFields(Fields: TFields; FieldDefs: TFieldDefs);
  var
    I: Integer;
    F: TField;
  begin
    for I := 0 to Fields.Count - 1 do
    begin
      F := Fields[I];
      with F do
        if FieldDefs.IndexOf(FieldName) = -1 then
        begin
          FieldDef := FieldDefs.AddFieldDef;
          FieldDef.Name := FieldName;
          FieldDef.DataType := DataType;
          FieldDef.Size := Size;
          if Required then
            FieldDef.Attributes := [faRequired];
          if ReadOnly then
            FieldDef.Attributes := FieldDef.Attributes + [DB.faReadonly];
          if (DataType = ftBCD) and (F is TBCDField) then
            FieldDef.Precision := TBCDField(F).Precision;
          if F is TObjectField then
            InitFieldDefsFromFields(TObjectField(F).Fields, FieldDef.ChildDefs);
        end;
    end;
  end;

begin
  FieldDefs.Clear;

  InitFieldDefsFromFields(Fields, FieldDefs);
end;

procedure TCustomVirtualDataset.InternalInitRecord(Buffer: TRecordBuffer);
var
  I: Integer;
begin
  for I := 0 to Fields.Count - 1 do
    PVariantList(Buffer + sizeof(TArrayRecInfo))[I] := Null;
end;

procedure TCustomVirtualDataset.InternalLast;
begin
  FCurrent := RecordCount;
end;

procedure TCustomVirtualDataset.InternalOpen;
begin
  FInternalOpen := True;
  FCurrent := -1;

  BookmarkSize := sizeof(Integer);

  FieldDefs.Updated := False;
  FieldDefs.Update;

  InternalCreateFields;
  Reserved := Pointer(FieldListCheckSum(Self));
  BindFields(True);
  FRecBufSize := sizeof(TArrayRecInfo) + (Fields.Count * sizeof(Variant));
end;

procedure TCustomVirtualDataset.InternalPost;
var
  RecBuf: TRecordBuffer;

begin
  UpdateCursorPos;
  GetActiveRecBuf(RecBuf);

  if PArrayRecInfo(RecBuf)^.BookmarkFlag = bfEof then
    DoPostData(-1)
  else
    DoPostData(PArrayRecInfo(RecBuf)^.Index);
end;

procedure TCustomVirtualDataset.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  if PArrayRecInfo(Buffer)^.BookmarkFlag in [bfCurrent, bfInserted] then
    FCurrent := PArrayRecInfo(Buffer)^.Index;
end;

function TCustomVirtualDataset.IsCursorOpen: Boolean;
begin
  Result := FInternalOpen;
end;

function TCustomVirtualDataset.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  P: Integer;

begin
  if Assigned(FOnLocate) then
  begin
    P := -1;
    FOnLocate(Self, KeyFields, KeyValues, Options, P);
    Result := P <> -1;
    if Result and (P <> FCurrent) then
    begin
      DoBeforeScroll;
      FCurrent := P;
      Resync([rmCenter]);
      DoAfterScroll;
    end;
  end
  else
    Result := False;
end;

function TCustomVirtualDataset.Lookup(
  const KeyFields: string;
  const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
  if Assigned(FOnLookupValue) then
  begin
    Result := Null;
    FOnLookupValue(Self, KeyFields, KeyValues, ResultFields, Result);
  end
  else
    Result := inherited Lookup(KeyFields, KeyValues, ResultFields);
end;

procedure TCustomVirtualDataset.MasterChanged(Sender: TObject);
begin
  if not Active then
    Exit;
  FCurrent := -1;
  Resync([]);
end;

procedure TCustomVirtualDataset.MasterDisabled(Sender: TObject);
begin
  if not Active then
    Exit;
  Resync([]);
end;

procedure TCustomVirtualDataset.SetBookmarkFlag(
  Buffer: TRecordBuffer;
  Value: TBookmarkFlag);
begin
  PArrayRecInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TCustomVirtualDataset.SetBookmarkData(
  Buffer: TRecordBuffer;
  Data: Pointer);
begin
  if PArrayRecInfo(Buffer)^.BookmarkFlag in [bfCurrent, bfInserted] then
    PArrayRecInfo(Buffer)^.Index := PInteger(Data)^
  else
    PArrayRecInfo(Buffer)^.Index := -1;
end;

procedure TCustomVirtualDataset.SetFieldData(
  Field: TField;
  Buffer: Pointer);
begin
  {$WARNINGS OFF}
  inherited;
  {$WARNINGS ON}
  InternalSetFieldData(Field, Buffer, true);
end;

procedure TCustomVirtualDataset.SetIndex(const Value: Integer);
begin
  if (Value < 0) or (Value >= RecordCount) then
    VirtualDatasetError(SIndexOutOfRange, Self);
  RecNo := Value + 1;
end;

procedure TCustomVirtualDataset.SetTopIndex(const Value: Integer);
begin
  ClearBuffers;

  FCurrent := Value;

  if GetRecord(Buffers[0], gmCurrent, True) = grOK then
  //
  // Only fetch next records when Eof and Bof are false
  //
  begin
    ActivateBuffers;
    GetNextRecords;
  end;

  DataEvent(deDataSetChange, 0);
end;

procedure TCustomVirtualDataset.SetMasterSource(const Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    DatabaseError(SCircularDataLink, Self);
  MasterDataLink.DataSource := Value;
end;

procedure TCustomVirtualDataset.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  Value := Min(max(Value, 1), RecordCount);
  if RecNo <> Value then
  begin
    DoBeforeScroll;
    FCurrent := Value - 1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

procedure TCustomVirtualDataset.InternalSetFieldData(
  const Field: TField;
  const Buffer: Pointer;
  const NativeFormat: Boolean);

  procedure BufferToVar(var Data: Variant);
  var
    DT: TDateTime;
  begin
    case Field.DataType of
      ftInterface:
        Data := IUnknown(Buffer^);
      ftIDispatch:
        Data := IDispatch(Buffer^);
      ftVariant:
        Data := Variant(Buffer^);
      ftstring, ftFixedChar, ftGuid:
        Data := Ansistring(PAnsiChar(Buffer));
      ftWidestring, ftFixedWideChar:
        Data := Widestring(PWideChar(Buffer));
      ftAutoInc, ftInteger:
        Data := Longint(Buffer^);
      ftSmallint:
        Data := SmallInt(Buffer^);
      ftWord:
        Data := Word(Buffer^);
      ftBoolean:
        Data := WordBool(Buffer^);
      ftFloat, ftCurrency:
        Data := Double(Buffer^);
      ftExtended:
        Data := Extended(Buffer^);
      ftBlob, ftMemo, ftGraphic, ftWideMemo:
        Data := Variant(Buffer^);

      ftDate, ftTime, ftDateTime:
        if NativeFormat then
        begin
          {$WARNINGS OFF}
          DataConvert(Field, Buffer, @DT, False);
          {$WARNINGS ON}
          Data := DT;
        end
        else
          Data := TDateTime(Buffer^);

      ftBCD:
        if NativeFormat then
          {$WARNINGS OFF}
          DataConvert(Field, Buffer, @TVarData(Data).VCurrency, False)
          {$WARNINGS ON}
        else
          Data := Currency(Buffer^);

      ftBytes, ftVarBytes:
        if NativeFormat then
          {$WARNINGS OFF}
          DataConvert(Field, Buffer, @Data, False)
          {$WARNINGS ON}
        else
          Data := Variant(Buffer^);

      ftLargeInt:
        begin
          TVarData(Data).VType := VT_DECIMAL;
          Decimal(Data).Lo64 := Int64(Buffer^);
        end;

    else
      DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[Field.DataType],
        Field.DisplayName]);
    end;
  end;

var
  Data: Variant;
  RecBuf: TRecordBuffer;

begin
  with Field do
  begin
    if not(State in dsWriteModes) then
      DatabaseError(SNotEditing, Self);
    GetActiveRecBuf(RecBuf);

    if FieldNo > 0 then
    begin
      if ReadOnly and not(State in [dsSetKey, dsFilter]) then
        DatabaseErrorFmt(SFieldReadOnly, [DisplayName]);

      {$WARNINGS OFF}
      Validate(Buffer);
      {$WARNINGS ON}

      if FModifiedFields.IndexOf(Field) = -1 then
      begin
        //PVariantList(FOldValueBuffer + sizeof(TArrayRecInfo))[Field.Index] :=
        //  Field.Value;
        FModifiedFields.Add(Field);
      end;
    end;

    if Buffer = nil then
      Data := Null
    else
      BufferToVar(Data);

    PVariantList(RecBuf + sizeof(TArrayRecInfo))[Field.Index] := Data;

    if not(State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, Longint(Field));
  end;
end;

procedure TCustomVirtualDataset.SetFieldData(
  Field: TField;
  Buffer: TValueBuffer);
begin
  inherited;
  SetFieldData(Field, Pointer(Buffer));
end;

end.

