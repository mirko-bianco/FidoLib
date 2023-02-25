program GridViewModelExample;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {MainView},
  Person in 'Person.pas',
  PeopleGridViewModel in 'PeopleGridViewModel.pas',
  PeopleViewModel in 'PeopleViewModel.pas',
  PeopleGridViewModel.Intf in 'PeopleGridViewModel.Intf.pas',
  PeopleViewModel.Intf in 'PeopleViewModel.Intf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainView, MainView);
  Application.Run;
end.
