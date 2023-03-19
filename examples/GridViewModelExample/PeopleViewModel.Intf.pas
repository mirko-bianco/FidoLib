unit PeopleViewModel.Intf;

interface

uses
  Fmx.Grid,

  Fido.DesignPatterns.Observable.Intf,

  PeopleGridViewModel.Intf;

type
  IPeopleViewModel = interface(IObservable)
    ['{08DC7F85-3C42-44C6-95FE-82B97184512A}']

    function OnGetFieldValue: TOnGetValue;
    function OnSetFieldValue: TOnSetValue;
    function OnSortByHeaderClick: THeaderClick;
    function OnSortByHeaderMove: TColumnMovedEvent;

    function Filter: string;
    procedure SetFilter(const Filter: string);

    procedure EditRecord;
    procedure AddRecord;
    procedure DeleteRecord;
  end;

implementation

end.
