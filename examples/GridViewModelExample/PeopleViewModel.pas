unit PeopleViewModel;

interface

uses
  System.SysUtils,

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

  PeopleGridViewModel,
  PeopleGridViewModel.Intf,
  PeopleViewModel.Intf,
  Person;

type
  TPeopleViewModel = class(TDelegatedObservable, IPeopleViewModel, IObserver)
  private
    FData: IDeepObservableList<IPerson>;
    FGridViewModel: IPeopleGridViewModel;
  public
    constructor Create(const Grid: TGrid; const Data: IDeepObservableList<IPerson>);
    destructor Destroy; override;

    function OnGetFieldValue: TOnGetValue;
    function OnSetFieldValue: TOnSetValue;
    function OnSortByHeaderClick: THeaderClick;
    function OnSortByHeaderMove: TColumnMovedEvent;

    procedure AddRecord;
    procedure EditRecord;
    procedure DeleteRecord;

    function Filter: string;
    procedure SetFilter(const Filter: string);

    procedure Notify(const Sender: IInterface; const Notification: INotification);
  end;

implementation

{ TPeopleViewModel }

constructor TPeopleViewModel.Create(
  const Grid: TGrid;
  const Data: IDeepObservableList<IPerson>);
begin
  inherited Create(nil);

  FGridViewModel := TPeopleGridViewModel.Create(Grid, Data);
  FGridViewModel.RegisterObserver(Self);
  FData := Data;
end;

destructor TPeopleViewModel.Destroy;
begin
  FGridViewModel.UnregisterObserver(Self);
  inherited;
end;

procedure TPeopleViewModel.AddRecord;
begin
  FData.Add(TPerson.Create(TGuid.NewGuid, TGuid.NewGuid.ToString, TGuid.NewGuid.ToString));
end;

procedure TPeopleViewModel.DeleteRecord;
begin
  FData.Remove(FGridViewModel.SelectedItem);
end;

procedure TPeopleViewModel.EditRecord;
begin
  var Item := FGridViewModel.SelectedItem;
  if not Assigned(ITem) then
    Exit;
  Item.SetFirstName(TGuid.NewGuid.ToString);
end;

function TPeopleViewModel.Filter: string;
begin
  Result := FGridViewModel.Filter;
end;

procedure TPeopleViewModel.Notify(
  const Sender: IInterface;
  const Notification: INotification);
begin
  Broadcast(Notification);
end;

function TPeopleViewModel.OnGetFieldValue: TOnGetValue;
begin
  Result := FGridViewModel.OnGetFieldValue;
end;

function TPeopleViewModel.OnSetFieldValue: TOnSetValue;
begin
  Result := FGridViewModel.OnSetFieldValue;
end;

function TPeopleViewModel.OnSortByHeaderClick: THeaderClick;
begin
  Result := FGridViewModel.OnSortByHeaderClick;
end;

function TPeopleViewModel.OnSortByHeaderMove: TColumnMovedEvent;
begin
  Result := FGridViewModel.OnSortByHeaderMove;
end;

procedure TPeopleViewModel.SetFilter(const Filter: string);
begin
  FGridViewModel.SetFilter(Filter);
end;

end.
