unit Fido.Gui.Fmx.Grid.ViewModel.Test;

interface

uses
  DUnitX.TestFramework,
  SysUtils,

  FMX.Grid,

  Spring,
  Spring.Collections,

  Fido.DesignPatterns.Observable.Intf,
  Fido.DesignPatterns.Observable.Delegated,
  Fido.Gui.Fmx.Grid.ViewModel,
  Fido.Gui.Fmx.Grid.ViewModel.Intf,
  Fido.Collections;

type
  IPerson = interface(IObservable)
    ['{059DE00F-087F-41A2-9B36-B3A32C03CBA6}']

    procedure SetId(const Value: Integer);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    function Id: Integer;
    function FirstName: string;
    function LastName: string;
  end;

  TPerson = class(TDelegatedObservable, IPerson)
  private
    FId: Integer;
    FFirstName: string;
    FLastName: string;
  public
    constructor Create(const Id: Integer; const FirstName: string; const LastName: string);

    procedure SetId(const Value: Integer);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    function Id: Integer;
    function FirstName: string;
    function LastName: string;
  end;

  TPersonReadonlyGridViewModel = class(TAbstractReadonlyGridViewModel<IPerson>)
  protected
    procedure GridGetValue(Sender: TObject; const Col, Row: Integer; var Value: TValue); override;
  end;

  IPersonReadonlyFilteredGridViewModel = interface(IReadonlyGridViewModel<IPerson>)
    ['{1BE42F59-56BC-4F01-A3EE-805AF685B065}']

    function Filter: string;
    procedure SetFilter(const Filter: string);
  end;

  TPersonReadonlyFilteredGridViewModel = class(TAbstractReadonlyGridViewModel<IPerson>, IPersonReadonlyFilteredGridViewModel)
  private
    FFilteredGridViewModel: IFilteredGridViewModelHelper<IPerson, string>;
  protected
    function DecorateData(const Data: IList<IPerson>): IList<IPerson>; override;
    procedure GridGetValue(Sender: TObject; const Col, Row: Integer; var Value: TValue); override;
  public
    constructor Create(const Grid: TGrid; const Data: IList<IPerson>);

    function Filter: string;
    procedure SetFilter(const Filter: string);
  end;

  IPersonReadonlySortedGridViewModel = interface(IReadonlyGridViewModel<IPerson>)
    ['{01CDBF62-E100-409C-9F83-A4762869390E}']

    function OnSortByHeaderClick: THeaderClick;
    function OnSortByHeaderMove: TColumnMovedEvent;
  end;

  TPersonReadonlySortedGridViewModel = class(TAbstractReadonlyGridViewModel<IPerson>, IPersonReadonlySortedGridViewModel)
  private
    FSortedGridViewModel: ISortedByColumnGridViewModelHelper<IPerson>;
  protected
    function DecorateData(const Data: IList<IPerson>): IList<IPerson>; override;
    procedure GridGetValue(Sender: TObject; const Col, Row: Integer; var Value: TValue); override;
  public
    constructor Create(const Grid: TGrid; const Data: IList<IPerson>);
    destructor Destroy; override;

    function OnSortByHeaderClick: THeaderClick;
    function OnSortByHeaderMove: TColumnMovedEvent;
  end;

  [TestFixture]
  TFmxGridViewModel = class
  public
    [Test]
    procedure ReadonlyWorks;

    [Test]
    procedure FilteredWorks;

    [Test]
    procedure SortedWorks;
  end;

const
  ID_COLUMN = 0;
  FIRSTNAME_COLUMN = 1;
  LASTNAME_COLUMN = 2;


implementation

{ TFmxGridViewModel }

procedure TFmxGridViewModel.FilteredWorks;
begin
  var List := TCollections.GetListOfDeepObservable<IPerson>;
  List.Add(TPerson.Create(1, 'Donald', 'Duck'));
  List.Add(TPerson.Create(2, 'Mickey', 'Mouse'));
  List.Add(TPerson.Create(3, 'Mirko', 'Bianco'));

  var Grid := TGrid.Create(nil);
  try
    var LastNameCol := TColumn.Create(Grid);

    Grid.Model.InsertColumn(ID_COLUMN, TIntegerColumn.Create(Grid));
    Grid.Model.InsertColumn(FIRSTNAME_COLUMN, TColumn.Create(Grid));
    Grid.Model.InsertColumn(LASTNAME_COLUMN, LastNameCol);
    Grid.columns[ID_COLUMN].Header := 'Id';
    Grid.columns[FIRSTNAME_COLUMN].Header := 'First name';
    Grid.columns[LASTNAME_COLUMN].Header := 'Last name';

    var GridViewModel: IPersonReadonlyFilteredGridViewModel := TPersonReadonlyFilteredGridViewModel.Create(Grid, List);

    GridViewModel.SetFilter('Mi');

    Grid.Row := 0;
    Assert.AreEqual(2, Grid.RowCount);
    Assert.AreEqual(List[1], GridViewModel.SelectedItem);

  finally
    Grid.Free;
  end;
end;

procedure TFmxGridViewModel.ReadonlyWorks;
begin
  var List := TCollections.GetListOfDeepObservable<IPerson>;
  List.Add(TPerson.Create(1, 'Donald', 'Duck'));
  List.Add(TPerson.Create(2, 'Mickey', 'Mouse'));
  List.Add(TPerson.Create(3, 'Mirko', 'Bianco'));

  var Grid := TGrid.Create(nil);
  try
    var LastNameCol := TColumn.Create(Grid);

    Grid.Model.InsertColumn(ID_COLUMN, TIntegerColumn.Create(Grid));
    Grid.Model.InsertColumn(FIRSTNAME_COLUMN, TColumn.Create(Grid));
    Grid.Model.InsertColumn(LASTNAME_COLUMN, LastNameCol);
    Grid.columns[ID_COLUMN].Header := 'Id';
    Grid.columns[FIRSTNAME_COLUMN].Header := 'First name';
    Grid.columns[LASTNAME_COLUMN].Header := 'Last name';

    var GridViewModel: IReadonlyGridViewModel<IPerson> := TPersonReadonlyGridViewModel.Create(Grid, List);

    Grid.Row := 2;
    Grid.Col := 1;

    Assert.AreEqual(Grid.Row, GridViewModel.SelectedRow);
    Assert.AreEqual(Grid.Col, GridViewModel.SelectedColumn);
    Assert.AreEqual(List[2], GridViewModel.SelectedItem);
  finally
    Grid.Free;
  end;
end;

procedure TFmxGridViewModel.SortedWorks;
begin
  var List := TCollections.GetListOfDeepObservable<IPerson>;
  List.Add(TPerson.Create(1, 'Donald', 'Duck'));
  List.Add(TPerson.Create(2, 'Mickey', 'Mouse'));
  List.Add(TPerson.Create(3, 'Mirko', 'Bianco'));

  var Grid := TGrid.Create(nil);
  try
    var LastNameCol := TColumn.Create(Grid);

    Grid.Model.InsertColumn(ID_COLUMN, TIntegerColumn.Create(Grid));
    Grid.Model.InsertColumn(FIRSTNAME_COLUMN, TColumn.Create(Grid));
    Grid.Model.InsertColumn(LASTNAME_COLUMN, LastNameCol);
    Grid.columns[ID_COLUMN].Header := 'Id';
    Grid.columns[FIRSTNAME_COLUMN].Header := 'First name';
    Grid.columns[LASTNAME_COLUMN].Header := 'Last name';

    var GridViewModel: IPersonReadonlySortedGridViewModel := TPersonReadonlySortedGridViewModel.Create(Grid, List);

    GridViewModel.OnSortByHeaderClick()(Grid.columns[LASTNAME_COLUMN]);

    Grid.Row := 0;
    Assert.AreEqual(3, Grid.RowCount);
    Assert.AreEqual(List[2], GridViewModel.SelectedItem);

  finally
    Grid.Free;
  end;
end;

{ TPerson }

constructor TPerson.Create(const Id: Integer; const FirstName, LastName: string);
begin
  inherited Create(nil);

  FId := Id;
  FFirstName := FirstName;
  FLastName := LastName;
end;

function TPerson.FirstName: string;
begin
  Result := FFirstName;
end;

function TPerson.Id: Integer;
begin
  Result := FId;
end;

function TPerson.LastName: string;
begin
  Result := FLastName;
end;

procedure TPerson.SetFirstName(const Value: string);
begin
  if FFirstName = Value then
    Exit;
  FFirstName := Value;
  Self.Broadcast('FirstName changed');
end;

procedure TPerson.SetId(const Value: Integer);
begin
  if FId = Value then
    Exit;
  FId := Value;
  Self.Broadcast('Id changed');
end;

procedure TPerson.SetLastName(const Value: string);
begin
  if FLastName = Value then
    Exit;
  FLastName := Value;
  Self.Broadcast('LastName changed');
end;

{ TPersonReadonlyGridViewModel }

procedure TPersonReadonlyGridViewModel.GridGetValue(Sender: TObject; const Col, Row: Integer; var Value: TValue);
begin
  inherited;
  case GetColumnIndexByCol(Col) of
    ID_COLUMN: Value := ViewData[Row].Id.ToString;
    FIRSTNAME_COLUMN: Value := ViewData[Row].FirstName;
    LASTNAME_COLUMN: Value := ViewData[Row].LastName;
  end;
end;

{ TPersonReadonlyFilteredGridViewModel }

constructor TPersonReadonlyFilteredGridViewModel.Create(const Grid: TGrid; const Data: IList<IPerson>);
begin
  FFilteredGridViewModel := TFilteredGridViewModelHelper<IPerson, string>.Create(function(const Rec: IPerson; const Filter: string): Boolean
    begin
      Result := Filter.IsEmpty or
        Rec.Id.ToString.ToLower.Contains(Filter.ToLower) or
        Rec.FirstName.ToLower.Contains(Filter.ToLower) or
        Rec.LastName.ToLower.Contains(Filter.ToLower);
    end);

  inherited Create(Grid, Data);
end;

function TPersonReadonlyFilteredGridViewModel.DecorateData(const Data: IList<IPerson>): IList<IPerson>;
begin
  Result := FFilteredGridViewModel.DecorateData(Data);
end;

function TPersonReadonlyFilteredGridViewModel.Filter: string;
begin
  Result := FFilteredGridViewModel.Filter;
end;

procedure TPersonReadonlyFilteredGridViewModel.GridGetValue(Sender: TObject;const Col, Row: Integer; var Value: TValue);
begin
  inherited;
  case GetColumnIndexByCol(Col) of
    ID_COLUMN: Value := ViewData[Row].Id.ToString;
    FIRSTNAME_COLUMN: Value := ViewData[Row].FirstName;
    LASTNAME_COLUMN: Value := ViewData[Row].LastName;
  end;
end;

procedure TPersonReadonlyFilteredGridViewModel.SetFilter(const Filter: string);
begin
  FFilteredGridViewModel.SetFilter(Filter);
  SetData(Data);
end;

{ TPersonReadonlySortedGridViewModel }

constructor TPersonReadonlySortedGridViewModel.Create(const Grid: TGrid; const Data: IList<IPerson>);
begin
  FSortedGridViewModel := TSortedByColumnGridViewModelHelper<IPerson>.Create(
    Grid,
    function(const Index: Integer; const Left: IPerson; const Right: IPerson): Integer
    begin
      case Index of
        ID_COLUMN: Result := CompareStr(Left.Id.ToString, Right.Id.ToString);
        FIRSTNAME_COLUMN: Result := CompareStr(Left.FirstName, Right.FirstName);
        LASTNAME_COLUMN: Result := CompareStr(Left.LastName, Right.LastName);
        else Result := 0
      end;
    end);

  inherited Create(Grid, Data);
  FSortedGridViewModel.RegisterObserver(Self);
end;

function TPersonReadonlySortedGridViewModel.DecorateData(const Data: IList<IPerson>): IList<IPerson>;
begin
  Result := FSortedGridViewModel.DecorateData(Data);
end;

destructor TPersonReadonlySortedGridViewModel.Destroy;
begin
  FSortedGridViewModel.UnregisterObserver(Self);
  inherited;
end;

procedure TPersonReadonlySortedGridViewModel.GridGetValue(Sender: TObject; const Col, Row: Integer; var Value: TValue);
begin
  inherited;
  case GetColumnIndexByCol(Col) of
    ID_COLUMN: Value := TValue.From<string>(ViewData[Row].Id.ToString);
    FIRSTNAME_COLUMN: Value := TValue.From<string>(ViewData[Row].FirstName);
    LASTNAME_COLUMN: Value := TValue.From<string>(ViewData[Row].LastName);
  end;
end;

function TPersonReadonlySortedGridViewModel.OnSortByHeaderClick: THeaderClick;
begin
  Result := FSortedGridViewModel.OnSortByHeaderClick;
end;

function TPersonReadonlySortedGridViewModel.OnSortByHeaderMove: TColumnMovedEvent;
begin
  Result := FSortedGridViewModel.OnSortByHeaderMove;
end;

initialization
  TDUnitX.RegisterTestFixture(TFmxGridViewModel);
end.
