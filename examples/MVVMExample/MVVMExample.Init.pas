unit MVVMExample.Init;

interface

uses
  System.Classes,
  System.SysUtils,

  FireDAC.Phys,
  FireDAC.Stan.Async,
  FireDAC.Stan.Intf,
  FireDac.Stan.Def,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite,
  FireDAC.UI.Intf,
  FireDAC.ConsoleUI.Wait,
  FireDAC.Comp.UI,
  FireDAC.Dapt,

  Vcl.Forms,

  Spring,
  Spring.Container,

  Fido.Mappers,
  Fido.Containers,
  Fido.Db.Connections.FireDac,
  Fido.StatementExecutor.Intf,
  Fido.Db.StatementExecutor.FireDac,
  Fido.VirtualQuery,

  Main.View,
  Song.Model,
  Song.DomainObject,
  Song.DomainObject.Intf,
  Song.Model.Intf,
  Song.View,
  Song.ViewModel,
  Song.ViewModel.Intf,
  Main.ViewModel.Intf,
  Main.ViewModel,
  Song.Repository.Data.Intf,
  Song.Repository.Intf,
  Song.Repository;

type
  MVVMExampleInitialization = class
  public
    class procedure InitializeFireDacCursor; static;
    class procedure Register(const Container: TContainer; const FireDacDatabaseParams: TStrings); static;
  end;

implementation

class procedure MVVMExampleInitialization.InitializeFireDacCursor;
var
  FDGUIxWaitCursor: TFDGUIxWaitCursor;
begin
  FDGUIxWaitCursor := TFDGUIxWaitCursor.Create(Application);
  FDGUIxWaitCursor.Provider := 'Console';
  FDGUIxWaitCursor.ScreenCursor := gcrNone;
end;

class procedure MVVMExampleInitialization.Register(const Container: TContainer; const FireDacDatabaseParams: TStrings);
begin
  Container.RegisterType<TFireDacConnections>.DelegateTo(
    function: TFireDacConnections
    begin
      Result := TFireDacConnections.Create(FireDacDatabaseParams);
    end).AsSingleton;
  Container.RegisterType<IStatementExecutor, TFireDacStatementExecutor>;

  Container.RegisterType<TMainView>.AsSingleton;
  Container.RegisterType<IMainViewModel, TMainViewModel>;
  Container.RegisterType<TSongView>;
  Container.RegisterInstance<TFunc<Integer, TSongView>>(
    function(Id: Integer): TSongView
    begin
      Result := Container.Resolve<TSongView>([TNamedValue.From<Integer>(Id, 'Id')]);
    end
  );
  Container.RegisterType<ISongViewModel, TSongViewModel>;
  Container.RegisterInstance<TFunc<Integer, ISongViewModel>>(
    function(Id: Integer): ISongViewModel
    begin
      Result := Container.Resolve<ISongViewModel>([TNamedValue.From<Integer>(Id, 'Id')]);
    end
  );
  Container.RegisterType<ISongModel, TSongModel>;

  Containers.RegisterVirtualQuery<ISongRecord, ISongListQuery>(Container);
  Containers.RegisterVirtualQuery<ISongRecord, ISongByIdQuery>(Container);
  Containers.RegisterVirtualStatement<ISongUpdateByIdCommand>(Container);

  Container.RegisterType<ISongRepository, TSongRepository>;

  Container.RegisterType<ISong, TSong>;

  Mappers.RegisterMapper<ISongRecord, ISong>(
    procedure(Source: ISongRecord; Destination: ISong)
    begin
      Destination.SetId(Source.Id);
      Destination.SetTitle(Source.Title);
    end);
end;

end.

