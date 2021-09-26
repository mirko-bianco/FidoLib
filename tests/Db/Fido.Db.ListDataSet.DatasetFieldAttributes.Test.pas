unit Fido.Db.ListDataSet.DatasetFieldAttributes.Test;

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

  Fido.Testing.Mock.Utils,
  Fido.Collections,
  Fido.Collections.DeepObservableList.Intf,
  Fido.DesignPatterns.Observable.Intf,
  Fido.DesignPatterns.Observable.Delegated,
  Fido.Db.ListDataset,
  Fido.Db.DatasetFieldAttributes.Intf,
  Fido.Db.DatasetFieldAttributes;

type
  ITypeTest = interface(IObservable)
    ['{2CEE93F6-7B3F-4E4B-B689-4C854609843C}']
    function GetInteger: Integer;
    procedure SetInteger(const Value: Integer);
  end;

  TTypeTest = class(TDelegatedObservable, ITypeTest)
  strict private
    FInteger: Integer;
  private
    function GetInteger: Integer;
    procedure SetInteger(const Value: Integer);
  public
    constructor Create(const AInteger: Integer); overload;
    constructor Create; overload;
  end;

  [TestFixture]
  TListDataSetTest = class(TObject)
  strict private
    FList: IDeepObservableList<ITypeTest>;
    FDataset: TListDataSet<ITypeTest>;

    FSomeLength: Integer;
    FSomeLabel: string;
    FSomeReadOnly: Boolean;
    FSomeMask: string;
    FSomeVisible: Boolean;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestIntegerField;
  end;

implementation

procedure TListDataSetTest.Setup;
var
  FieldAttributes: IDatasetFieldAttributes;
begin
  FSomeLength := MockUtils.WithIntegerRange(0, 50).SomeInteger;
  FSomeLabel := MockUtils.SomeString;
  FSomeReadOnly := MockUtils.SomeBoolean;
  FSomeMask :=  MockUtils.SomeString;
  FSomeVisible := MockUtils.SomeBoolean;

  FieldAttributes := TDatasetFieldAttributes.Create;
  FieldAttributes.SetAttribute('Integer', FSomeLength, FSomeLabel, FSomeReadOnly, FSomeMask, FSomeVisible);

  FDataset := TListDataSet<ITypeTest>.Create(
    nil,
    function(TypeInfo: PTypeInfo): TValue
    begin
      if TypeInfo.TypeName.ToUpper.Equals('ITYPETEST') then
        Result := TValue.From<ITypeTest>(TTypeTest.Create)
    end,
    FieldAttributes);

  FList := Fido.Collections.TCollections.GetListOfDeepObservable<ITypeTest>;

  FList.Add(TTypeTest.Create(1));

  FDataset.DataList := FList;
  FDataset.Open;
end;

procedure TListDataSetTest.TearDown;
begin
  FDataset.Free;
end;

procedure TListDataSetTest.TestIntegerField;
begin
  Assert.AreEqual(FSomeLength, FDataset.FindField('Integer').DisplayWidth);
  Assert.AreEqual(FSomeLabel, FDataset.FindField('Integer').DisplayLabel);
  Assert.AreEqual(FSomeReadOnly, FDataset.FindField('Integer').ReadOnly);
  Assert.AreEqual(FSomeMask, FDataset.FindField('Integer').EditMask);
  Assert.AreEqual(FSomeVisible, FDataset.FindField('Integer').Visible);
end;

{ TTypeTest }

constructor TTypeTest.Create(const AInteger: Integer);
begin
  inherited Create(self);

  FInteger := AInteger;
end;

constructor TTypeTest.Create;
begin
  inherited Create(self);

  FInteger := 0;
end;

function TTypeTest.GetInteger: Integer;
begin
  Result := FInteger;
end;

procedure TTypeTest.SetInteger(const Value: Integer);
begin
  Observables.SetAndBroadcast<Integer>(Self, FInteger, Value, 'Changed Integer', TValue.From<ITypeTest>(Self));
end;

initialization
  TDUnitX.RegisterTestFixture(TListDataSetTest);
end.
