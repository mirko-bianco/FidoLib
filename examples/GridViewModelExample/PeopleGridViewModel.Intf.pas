unit PeopleGridViewModel.Intf;

interface

uses
  Fmx.Grid,

  Fido.Gui.Fmx.Grid.ViewModel.Intf,

  Person;

type
  IPeopleGridViewModel = interface(IWriteableGridViewModel<IPerson>)
    ['{A58C65A0-4163-4533-B8FA-F7D44B39113F}']

    function Filter: string;
    procedure SetFilter(const Filter: string);

    function OnSortByHeaderClick: THeaderClick;
    function OnSortByHeaderMove: TColumnMovedEvent;
  end;

implementation

end.
