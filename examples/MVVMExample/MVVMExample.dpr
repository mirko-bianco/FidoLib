program MVVMExample;

{$R 'Queries.res' 'Repositories\Queries.rc'}

uses
  System.Classes,
  System.SysUtils,
  Vcl.Forms,
  Spring,
  Spring.Container,
  Fido.Registration,
  Fido.Db.Connections.FireDac,
  MVVMExample.Init in 'MVVMExample.Init.pas',
  Main.View in 'Views\Main.View.pas' {MainView},
  Song.View in 'Views\Song.View.pas' {SongView},
  Main.ViewModel.Intf in 'ViewModels\Main.ViewModel.Intf.pas',
  Main.ViewModel in 'ViewModels\Main.ViewModel.pas',
  Song.ViewModel.Intf in 'ViewModels\Song.ViewModel.Intf.pas',
  Song.ViewModel in 'ViewModels\Song.ViewModel.pas',
  Song.Model.Intf in 'Models\Song.Model.Intf.pas',
  Song.Model in 'Models\Song.Model.pas',
  Song.DomainObject.Intf in 'DomainObjects\Song.DomainObject.Intf.pas',
  Song.DomainObject in 'DomainObjects\Song.DomainObject.pas',
  Song.Repository.Data.Intf in 'Repositories\Song.Repository.Data.Intf.pas',
  Song.Repository.Intf in 'Repositories\Song.Repository.Intf.pas',
  Song.Repository in 'Repositories\Song.Repository.pas';

{$R *.res}
{$R 'Queries.res' 'Repositories\Queries.rc'}

var
  Container: Shared<TContainer>;
  MainView: TMainView;
  Connections: TFireDacConnections;
  FireDacDatabaseParams: Shared<TStringList>;
begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Container := TContainer.Create;

  Registration.RegisterFramework(Container);

  FireDacDatabaseParams := TStringList.Create;
  FireDacDatabaseParams.Value.Add('DriverID=SQLite');
  MVVMExampleInitialization.Register(Container, FireDacDatabaseParams);
  Container.Value.Build;

  Application.Initialize;
  MVVMExampleInitialization.InitializeFireDacCursor;

  //Add some random data to the database
  Connections := Container.Value.Resolve<TFireDacConnections>;
  Connections.GetCurrent.ExecSQL('create table songs (title text not null);');
  Connections.GetCurrent.ExecSQL('insert into songs (title) values ("Carmina burana");');
  Connections.GetCurrent.ExecSQL('insert into songs (title) values ("Happy birthday");');

  MainView := Container.Value.Resolve<TMainView>([TNamedValue.From<TComponent>(Application, 'Owner')]);

  Pointer((@Application.MainForm)^) := MainView;
  Application.MainFormOnTaskbar := True;
  MainView.Show;

  Application.Run;
end.
