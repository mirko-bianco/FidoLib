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
  FireDAC.Comp.Client,

  Vcl.Forms,

  Spring,
  Spring.Container,

  Fido.Mappers,
  Fido.Containers,
  Fido.Db.Connections.FireDac,
  Fido.Db.Connections.FireDac.PerThread,
  Fido.StatementExecutor.Intf,
  Fido.Db.StatementExecutor.FireDac,

  Main.View,
  Main.ViewModel.Intf,
  Main.ViewModel,
  Song.DomainObject,
  Song.DomainObject.Intf,
  Song.Repository.Data.Intf,
  Song.Repository.Intf,
  Song.Repository,
  Song.Model.Intf,
  Song.Model,
  Song.View,
  Song.ViewModel.Intf,
  Song.ViewModel,
  System.Generics.Collections;

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
  Container.RegisterType<TFireDacConnections>(
    function: TFireDacConnections
    begin
      Result := TFireDacPerThreadConnections.Create(FireDacDatabaseParams);
    end).AsSingleton;

  Container.RegisterType<IStatementExecutor, TFireDacStatementExecutor>;

  Container.RegisterType<TMainView>.AsSingleton;
  Container.RegisterType<IMainViewModel, TMainViewModel>;
  Container.RegisterType<TSongView>;
  Container.RegisterInstance<TFunc<ISongViewModel, TSongView>>(
    function(SongViewModel: ISongViewModel): TSongView
    begin
      Result := Container.Resolve<TSongView>([TNamedValue.From<ISongViewModel>(SongViewModel, 'SongViewModel')]);
    end
  );
  Container.RegisterType<ISongViewModel, TSongViewModel>;
  Container.RegisterInstance<TFunc<Integer, ISongModel, ISongViewModel>>(
    function(Id: Integer;  SongModel: ISongModel): ISongViewModel
    begin
      Result := Container.Resolve<ISongViewModel>([TNamedValue.From<Integer>(Id, 'Id'), TNamedValue.From<ISongModel>(SongModel, 'SongModel')]);
    end
  );
  Container.RegisterType<ISongModel, TSongModel>;

  Containers.RegisterVirtualQuery<ISongRecord, ISongListQuery>(Container);
  Containers.RegisterVirtualQuery<ISongRecord, ISongByIdQuery>(Container);
  Containers.RegisterVirtualStatement<ISongUpdateByIdCommand>(Container);
  Containers.RegisterVirtualStatement<ISongInsertCommand>(Container);
  Containers.RegisterVirtualQuery<IIdRecord, ISongLastIdQuery>(Container);
  Containers.RegisterVirtualStatement<ISongDeleteByIdCommand>(Container);

  Container.RegisterType<ISongRepository, TSongRepository>;

  Container.RegisterType<ISong, TSong>;

  Mappers.RegisterMapper<ISongRecord, ISong>(
    procedure(const Source: ISongRecord; var Destination: ISong)
    begin
      Destination.SetId(Source.Id);
      Destination.SetTitle(Source.Title);
    end);
end;

end.

