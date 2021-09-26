unit Fido.Db.ListDataSet.Test;

interface
uses
  System.Rtti,
  System.TypInfo,
  System.SysUtils,
  System.DateUtils,
  System.Classes,
  System.Variants,

  Data.DB,

  DUnitX.TestFramework,

  Spring,
  Spring.Collections,

  Fido.Collections,
  Fido.Collections.DeepObservableList.Intf,
  Fido.DesignPatterns.Observable.Intf,
  Fido.DesignPatterns.Observable.Delegated,
  Fido.Db.ListDataset;

type
  ITypeTest = interface(IObservable)
    ['{2CEE93F6-7B3F-4E4B-B689-4C854609843C}']
    function GetInteger: Integer;
    procedure SetInteger(const Value: Integer);
    function GetDouble: Double;
    procedure SetDouble(const Value: Double);
    function GetString: string;
    procedure SetString(const Value: string);
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
    function GetBoolean: Boolean;
    procedure SetBoolean(const Value: Boolean);
    function GetExtended: Extended;
    procedure SetExtended(const Value: Extended);
  end;

  TTypeTest = class(TDelegatedObservable, ITypeTest)
  strict private
    FInteger: Integer;
    FDouble: Double;
    FString: string;
    FDate: TDateTime;
    FBoolean: Boolean;
    FExtended: Extended;
  private
    function GetInteger: Integer;
    procedure SetInteger(const Value: Integer);
    function GetDouble: Double;
    procedure SetDouble(const Value: Double);
    function GetString: string;
    procedure SetString(const Value: string);
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
    function GetBoolean: Boolean;
    procedure SetBoolean(const Value: Boolean);
    function GetExtended: Extended;
    procedure SetExtended(const Value: Extended);
  public
    constructor Create(const AInteger: Integer; const ADouble: Double; const AString: string; const ADate: TDateTime; const ABoolean: Boolean; const AExtended: Extended); overload;
    constructor Create; overload;
  end;

  [TestFixture]
  TListDataSetTest = class(TObject)
  strict private
    FList: IDeepObservableList<ITypeTest>;
    FDataset: TListDataSet<ITypeTest>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure RecordCountReturnstheListCount;
    [Test]
    procedure TestDefaultData;
    [Test]
    procedure CurrentEntityReturnsTheItemAtSamePosition;
    [Test]
    procedure AppendAndPostAddsARecordAtTheEndOfTheList;
    [Test]
    procedure EditAndPostChangesTheListItem;
    [Test]
    procedure OneFieldLocateFailsWhenTheValueIsNotCorrect;
    [Test]
    procedure OneFieldCaseInsensitiveLocateFailsWhenTheValueIsNotCorrect;
    [Test]
    procedure OneFieldCaseInsensitiveAndPartialKeyLocateFailsWhenTheValueIsNotCorrect;
    [Test]
    procedure OneFieldPartialKeyLocateFailsWhenTheValueIsNotCorrect;
    [Test]
    procedure OneFieldLocateWorksWhenTheValueIsCorrect;
    [Test]
    procedure OneFieldCaseInsensitiveLocateWorksWhenTheValueIsCorrect;
    [Test]
    procedure OneFieldCaseInsensitiveAndPartialKeyLocateWorksWhenTheValueIsCorrect;
    [Test]
    procedure OneFieldPartialKeyLocateWorksWhenTheValueIsCorrect;
    [Test]
    procedure MultipleFieldsLocateFailsWhenTheValueIsNotCorrect;
    [Test]
    procedure MultipleFieldsCaseInsensitiveLocateFailsWhenTheValueIsNotCorrect;
    [Test]
    procedure MultipleFieldsCaseInsensitiveAndPartialKeyLocateFailsWhenTheValueIsNotCorrect;
    [Test]
    procedure MultipleFieldsPartialKeyLocateFailsWhenTheValueIsNotCorrect;
    [Test]
    procedure MultipleFieldsLocateWorksWhenTheValueIsCorrect;
    [Test]
    procedure MultipleFieldsCaseInsensitiveLocateWorksWhenTheValueIsCorrect;
    [Test]
    procedure MultipleFieldsCaseInsensitiveAndPartialKeyLocateWorksWhenTheValueIsCorrect;
    [Test]
    procedure MultipleFieldsPartialKeyLocateWorksWhenTheValueIsCorrect;
    [Test]
    procedure SingleFieldLookupWorksWhenTheValueIsCorrect;
    [Test]
    procedure SingleFieldLookupFailsWhenTheValueIsNotCorrect;
    [Test]
    procedure MultipleFieldsLookupWorksWhenTheValueIsCorrect;
    [Test]
    procedure MultipleFieldsLookupFailsWhenTheValueIsCorrect;
  end;

implementation

procedure TListDataSetTest.Setup;
begin
  FDataset := TListDataSet<ITypeTest>.Create(
    nil,
    function(TypeInfo: PTypeInfo): TValue
    begin
      if TypeInfo.TypeName.ToUpper.Equals('ITYPETEST') then
        Result := TValue.From<ITypeTest>(TTypeTest.Create)
    end);

  FList := TCollections.GetListOfDeepObservable<ITypeTest>;

  FList.Add(TTypeTest.Create(1, 1.5, 'Bill', EncodeDateDay(1977, 32), True, 20.1));
  FList.Add(TTypeTest.Create(2, 2.5, 'John', EncodeDateDay(1976, 120), True, 20.2));
  FList.Add(TTypeTest.Create(3, 3.5, 'Matt', Date, False, 20.3));

  FDataset.DataList := FList;
  FDataset.Open;
end;

procedure TListDataSetTest.TearDown;
begin
  FDataset.Free;
end;

procedure TListDataSetTest.CurrentEntityReturnsTheItemAtSamePosition;
var
  RecPos: Integer;
begin
  RecPos := 0;
  FDataSet.First;
  while not FDataSet.Eof do
  begin
    Assert.AreSame(FDataSet.CurrentEntity, FList[RecPos]);
    FDataset.Next;
    Inc(RecPos);
  end;
end;

procedure TListDataSetTest.TestDefaultData;
var
  RecPos: Integer;
begin
  RecPos := 0;
  FDataSet.First;
  while not FDataSet.Eof do
  begin
    Assert.AreEqual(FDataSet.FieldByName('Integer').AsInteger, FList[RecPos].GetInteger);
    Assert.AreEqual(FDataSet.FieldByName('String').AsString, FList[RecPos].GetString);
    Assert.AreEqual(FDataSet.FieldByName('Double').AsFloat, FList[RecPos].GetDouble);
    Assert.AreEqual(FDataSet.FieldByName('Date').AsDateTime, FList[RecPos].GetDate);
    Assert.AreEqual(FDataSet.FieldByName('Boolean').AsBoolean, FList[RecPos].GetBoolean);
    Assert.AreEqual(FDataSet.FieldByName('Extended').AsExtended, FList[RecPos].GetExtended);
    FDataset.Next;
    Inc(RecPos);
  end;
end;

procedure TListDataSetTest.EditAndPostChangesTheListItem;
var
  TestDate: TDateTime;
begin
  FDataset.Last;
  FDataset.Edit;
  TestDate := Date + 1;
  FDataset.FindField('Date').AsDateTime := TestDate;
  FDataSet.Post;
  Assert.AreEqual(FDataSet.FieldByName('Date').AsDateTime, TestDate);
end;

procedure TListDataSetTest.AppendAndPostAddsARecordAtTheEndOfTheList;
var
  RecCount: Integer;
begin
  RecCount := FDataSet.RecordCount;
  FDataset.Append;
  FDataSet.FieldByName('Integer').AsInteger := 100;
  FDataSet.FieldByName('String').AsString := 'Test';
  FDataSet.FieldByName('Date').AsDateTime := EncodeDateDay(2017, 1);
  FDataSet.FieldByName('Double').AsFloat := 100.5;
  FDataSet.FieldByName('Boolean').AsBoolean := True;
  FDataSet.FieldByName('Extended').Value := 21.6;
  FDataset.Post;

  Assert.AreEqual(RecCount + 1, FDataSet.RecordCount);
  Assert.AreEqual(FDataSet.FieldByName('Integer').AsInteger, 100);
  Assert.AreEqual(FDataSet.FieldByName('String').AsString, 'Test');
  Assert.AreEqual(FDataSet.FieldByName('Date').AsDateTime, EncodeDateDay(2017, 1));
  Assert.AreEqual<Double>(FDataSet.FieldByName('Double').AsFloat, 100.5);
  Assert.AreEqual(FDataSet.FieldByName('Boolean').AsBoolean, True);
  Assert.AreEqual(FDataSet.FieldByName('Extended').AsExtended, 21.6);
end;

procedure TListDataSetTest.OneFieldCaseInsensitiveLocateFailsWhenTheValueIsNotCorrect;
begin
  Assert.IsFalse(FDataSet.Locate('string', 'johnny', [loCaseInsensitive]));
end;

procedure TListDataSetTest.OneFieldCaseInsensitiveAndPartialKeyLocateFailsWhenTheValueIsNotCorrect;
begin
  Assert.IsFalse(FDataSet.Locate('string', 'johnny', [loCaseInsensitive, loPartialKey]));
end;

procedure TListDataSetTest.OneFieldCaseInsensitiveLocateWorksWhenTheValueIsCorrect;
begin
  Assert.IsTrue(FDataSet.Locate('string', 'john', [loCaseInsensitive]));
end;

procedure TListDataSetTest.OneFieldCaseInsensitiveAndPartialKeyLocateWorksWhenTheValueIsCorrect;
begin
  Assert.IsTrue(FDataSet.Locate('string', 'jo', [loCaseInsensitive, loPartialKey]));
end;

procedure TListDataSetTest.OneFieldLocateFailsWhenTheValueIsNotCorrect;
begin
  Assert.IsFalse(FDataSet.Locate('string', 'john', []));
end;

procedure TListDataSetTest.OneFieldLocateWorksWhenTheValueIsCorrect;
begin
  Assert.IsTrue(FDataSet.Locate('string', 'John', []));
end;

procedure TListDataSetTest.OneFieldPartialKeyLocateFailsWhenTheValueIsNotCorrect;
begin
  Assert.IsFalse(FDataSet.Locate('string', 'jo', [loPartialKey]));
end;

procedure TListDataSetTest.OneFieldPartialKeyLocateWorksWhenTheValueIsCorrect;
begin
  Assert.IsTrue(FDataSet.Locate('string', 'Jo', [loPartialKey]));
end;

procedure TListDataSetTest.RecordCountReturnstheListCount;
begin
  Assert.IsTrue(FDataSet.RecordCount = FList.Count);
end;

procedure TListDataSetTest.MultipleFieldsPartialKeyLocateFailsWhenTheValueIsNotCorrect;
begin
  Assert.IsFalse(FDataSet.Locate('Integer;string', VarArrayOf([2, 'Ja']), [loPartialKey]));
end;

procedure TListDataSetTest.MultipleFieldsPartialKeyLocateWorksWhenTheValueIsCorrect;
begin
  Assert.IsTrue(FDataSet.Locate('Integer;string', VarArrayOf([2, 'Jo']), [loPartialKey]));
end;

procedure TListDataSetTest.MultipleFieldsCaseInsensitiveAndPartialKeyLocateWorksWhenTheValueIsCorrect;
begin
  Assert.IsTrue(FDataSet.Locate('Integer;string', VarArrayOf([2, 'jo']), [loCaseInsensitive, loPartialKey]));
end;

procedure TListDataSetTest.MultipleFieldsCaseInsensitiveLocateFailsWhenTheValueIsNotCorrect;
begin
  Assert.IsFalse(FDataSet.Locate('Integer;string', VarArrayOf([2, 'jahn']), [loPartialKey]));
end;

procedure TListDataSetTest.MultipleFieldsCaseInsensitiveLocateWorksWhenTheValueIsCorrect;
begin
  Assert.IsTrue(FDataSet.Locate('Integer;string', VarArrayOf([2, 'john']), [loCaseInsensitive]));
end;

procedure TListDataSetTest.MultipleFieldsCaseInsensitiveAndPartialKeyLocateFailsWhenTheValueIsNotCorrect;
begin
  Assert.IsFalse(FDataSet.Locate('Integer;string', VarArrayOf([2, 'Ja']), [loCaseInsensitive, loPartialKey]));
end;

procedure TListDataSetTest.MultipleFieldsLocateFailsWhenTheValueIsNotCorrect;
begin
  Assert.IsFalse(FDataSet.Locate('Integer;string', VarArrayOf([2, 'Ja']), [loPartialKey]));
end;

procedure TListDataSetTest.MultipleFieldsLocateWorksWhenTheValueIsCorrect;
begin
  Assert.IsTrue(FDataSet.Locate('Integer;string', VarArrayOf([2, 'John']), []));
end;

procedure TListDataSetTest.MultipleFieldsLookupFailsWhenTheValueIsCorrect;
begin
  Assert.IsNull(FDataSet.Lookup('string;Integer', VarArrayOf(['jahn', 2]), 'Double'));
end;

procedure TListDataSetTest.MultipleFieldsLookupWorksWhenTheValueIsCorrect;
begin
  Assert.AreEqual(2.5, Double(FDataSet.Lookup('string;Integer', VarArrayOf(['John', 2]), 'Double')), 0.1);
end;

procedure TListDataSetTest.SingleFieldLookupFailsWhenTheValueIsNotCorrect;
begin
  Assert.IsNull(FDataSet.Lookup('string', 'jahn', 'Integer'));
end;

procedure TListDataSetTest.SingleFieldLookupWorksWhenTheValueIsCorrect;
begin
  Assert.AreEqual(2, Integer(FDataSet.Lookup('string', 'John', 'Integer')));
end;

{ TTypeTest }

constructor TTypeTest.Create(const AInteger: Integer; const ADouble: Double; const AString: string; const ADate: TDateTime; const ABoolean: Boolean; const AExtended: Extended);
begin
  inherited Create(Self);

  FInteger := AInteger;
  FDouble := ADouble;
  FString := AString;
  FDate := ADate;
  FBoolean := ABoolean;
  FExtended := AExtended;
end;

constructor TTypeTest.Create;
begin
  inherited Create(Self);

  FInteger := 0;
  FDouble := 0.0;
  FString := '';
  FDate := Now;
  FBoolean := False;
  FExtended := 0.0;
end;

function TTypeTest.GetBoolean: Boolean;
begin
  Result := FBoolean;
end;

function TTypeTest.GetDate: TDateTime;
begin
  Result := FDate;
end;

function TTypeTest.GetDouble: Double;
begin
  Result := FDouble;
end;

function TTypeTest.GetExtended: Extended;
begin
  Result := FExtended;
end;

function TTypeTest.GetInteger: Integer;
begin
  Result := FInteger;
end;

function TTypeTest.GetString: string;
begin
  Result := FString;
end;

procedure TTypeTest.SetBoolean(const Value: Boolean);
begin
  Observables.SetAndBroadcast<Boolean>(Self, FBoolean, Value, 'Changed Boolean', TValue.From<ITypeTest>(Self));
end;

procedure TTypeTest.SetDate(const Value: TDateTime);
begin
  Observables.SetAndBroadcast<TDateTime>(Self, FDate, Value, 'Changed Date', TValue.From<ITypeTest>(Self));
end;

procedure TTypeTest.SetDouble(const Value: Double);
begin
  Observables.SetAndBroadcast<Double>(Self, FDouble, Value, 'Changed Double', TValue.From<ITypeTest>(Self));
end;

procedure TTypeTest.SetExtended(const Value: Extended);
begin
  Observables.SetAndBroadcast<Extended>(Self, FExtended, Value, 'Changed Extended', TValue.From<ITypeTest>(Self));
end;

procedure TTypeTest.SetInteger(const Value: Integer);
begin
  Observables.SetAndBroadcast<Integer>(Self, FInteger, Value, 'Changed Integer', TValue.From<ITypeTest>(Self));
end;

procedure TTypeTest.SetString(const Value: string);
begin
  Observables.SetAndBroadcast<string>(Self, FString, Value, 'Changed String', TValue.From<ITypeTest>(Self));
end;

initialization
  TDUnitX.RegisterTestFixture(TListDataSetTest);
end.
