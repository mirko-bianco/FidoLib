unit PeopleGridViewModel;

interface

uses
  System.SysUtils,
  System.Generics.Defaults,

  Fmx.Grid,

  Spring,
  Spring.Collections,

  Fido.Utilities,
  Fido.Collections,
  Fido.Collections.DeepObservableList.Intf,
  Fido.DesignPatterns.Observer.Notification.Intf,
  Fido.DesignPatterns.Observer.Intf,
  Fido.DesignPatterns.Observable.Intf,
  Fido.DesignPatterns.Observable.Delegated,

  Fido.Gui.Fmx.Grid.ViewModel,
  Fido.Gui.Fmx.Grid.ViewModel.Intf,

  PeopleGridViewModel.Intf,
  Person;

type
  TPeopleGridViewModel = class(TAbstractWriteableGridView<IPerson>, IPeopleGridViewModel)
  strict private
    FFilteredGridViewModel: IFilteredGridViewModelHelper<IPerson, string>;
    FSortedGridViewModel: ISortedByColumnGridViewModelHelper<IPerson>;
  protected
    function DecorateData(const Data: IList<IPerson>): IList<IPerson>; override;
  public
    constructor Create(const Grid: TGrid; const Data: IList<IPerson>);
    destructor Destroy; override;

    procedure GridGetValue(Sender: TObject; const Col, Row: Integer; var Value: TValue); override;
    procedure GridSetValue(Sender: TObject; const Col, Row: Integer; const Value: TValue); override;

    function Filter: string;
    procedure SetFilter(const Filter: string);

    function OnSortByHeaderClick: THeaderClick;
    function OnSortByHeaderMove: TColumnMovedEvent;
  end;

const
  ID_COLUMN = 0;
  FIRSTNAME_COLUMN = 1;
  LASTNAME_COLUMN = 2;

implementation

{ TPeopleGridViewModel }

procedure TPeopleGridViewModel.GridGetValue(
  Sender: TObject;
  const Col, Row: Integer;
  var Value: TValue);
begin
  case GetColumnIndexByCol(Col) of
    ID_COLUMN: Value := TValue.From<string>(ViewData[Row].Id.ToString);
    FIRSTNAME_COLUMN: Value := TValue.From<string>(ViewData[Row].FirstName);
    LASTNAME_COLUMN: Value := TValue.From<string>(ViewData[Row].LastName);
  end;
end;

procedure TPeopleGridViewModel.GridSetValue(
  Sender: TObject;
  const Col, Row: Integer;
  const Value: TValue);
begin
  case GetColumnIndexByCol(Col) of
    ID_COLUMN: ViewData[Row].SetId(TGuid.Create(Value.AsString));
    FIRSTNAME_COLUMN: ViewData[Row].SetFirstName(Value.AsString);
    LASTNAME_COLUMN: ViewData[Row].SetLastName(Value.AsString);
  end;
end;

function TPeopleGridViewModel.Filter: string;
begin
  Result := FFilteredGridViewModel.Filter;
end;

function TPeopleGridViewModel.OnSortByHeaderClick: THeaderClick;
begin
  Result := FSortedGridViewModel.OnSortByHeaderClick;
end;

function TPeopleGridViewModel.OnSortByHeaderMove: TColumnMovedEvent;
begin
  Result := FSortedGridViewModel.OnSortByHeaderMove;
end;

procedure TPeopleGridViewModel.SetFilter(const Filter: string);
begin
  FFilteredGridViewModel.SetFilter(Filter);
  SetData(Data);
end;

constructor TPeopleGridViewModel.Create(
  const Grid: TGrid;
  const Data: IList<IPerson>);
begin
  FFilteredGridViewModel := TFilteredGridViewModelHelper<IPerson, string>.Create(function(const Rec: IPerson; const Filter: string): Boolean
    begin
      Result := Filter.IsEmpty or
        Rec.Id.ToString.ToLower.Contains(Filter.ToLower) or
        Rec.FirstName.ToLower.Contains(Filter.ToLower) or
        Rec.LastName.ToLower.Contains(Filter.ToLower);
    end);

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

function TPeopleGridViewModel.DecorateData(const Data: IList<IPerson>): IList<IPerson>;
begin
  Result := FSortedGridViewModel.DecorateData(FFilteredGridViewModel.DecorateData(Data));
end;

destructor TPeopleGridViewModel.Destroy;
begin
  FSortedGridViewModel.UnregisterObserver(Self);
  inherited;
end;

end.
