unit Song.Repository;

interface

uses
  System.SysUtils,

  Spring,
  Spring.Collections,

  Fido.Exceptions,
  Fido.Collections.DeepObservableList.Intf,
  Fido.Collections,
  Fido.Mappers,
  Fido.VirtualQuery,

  Song.Repository.Intf,
  Song.Repository.Data.Intf,
  Song.DomainObject.Intf;

type
  ESongRepository = class(EFidoException);

  TSongRepository = class(TInterfacedObject, ISongRepository)
  private
    FSongListQuery: ISongListQuery;
    FSongByIdQuery: ISongByIdQuery;
    FSongUpdateByIdCommand: ISongUpdateByIdCommand;
    FSongInsertCommand: ISongInsertCommand;
    FSongLastIdQuery: ISongLastIdQuery;
    FSongDeleteByIdCommand: ISongDeleteByIdCommand;
    FSongFactoryFunc: TFunc<ISong>;
  public
    constructor Create(
      const SongListQuery: ISongListQuery;
      const SongByIdQuery: ISongByIdQuery;
      const SongUpdateByIdCommand: ISongUpdateByIdCommand;
      const SongInsertCommand: ISongInsertCommand;
      const SongLastIdQuery: ISongLastIdQuery;
      const SongDeleteByIdCommand: ISongDeleteByIdCommand;
      const SongFactoryFunc: TFunc<ISong>);

    function GetNew: ISong;
    function GetList: IDeepObservableList<ISong>;
    function GetById(const Id: Integer): ISong;
    procedure Update(const Song: ISong);
    function Store(const Song: ISong): ISong;
    procedure Delete(const Id: Integer);
  end;

implementation

{ TSongRepository }

constructor TSongRepository.Create(
  const SongListQuery: ISongListQuery;
  const SongByIdQuery: ISongByIdQuery;
  const SongUpdateByIdCommand: ISongUpdateByIdCommand;
  const SongInsertCommand: ISongInsertCommand;
  const SongLastIdQuery: ISongLastIdQuery;
  const SongDeleteByIdCommand: ISongDeleteByIdCommand;
  const SongFactoryFunc: TFunc<ISong>);
begin
  Guard.CheckNotNull(SongListQuery, 'SongListQuery');
  Guard.CheckNotNull(SongByIdQuery, 'SongListById');
  Guard.CheckNotNull(SongUpdateByIdCommand, 'SongUpdateByIdCommand');
  Guard.CheckNotNull(SongInsertCommand, 'SongInsertCommand');
  Guard.CheckNotNull(SongLastIdQuery, 'ISongLastIdQuery');
  Guard.CheckNotNull(SongDeleteByIdCommand, 'SongDeleteByIdCommand');
  Guard.CheckTrue(Assigned(SongFactoryFunc), 'FSongFactoryFunc is not assigned');
  inherited Create;
  FSongListQuery := SongListQuery;
  FSongByIdQuery := SongByIdQuery;
  FSongUpdateByIdCommand := SongUpdateByIdCommand;
  FSongInsertCommand := SongInsertCommand;
  FSongLastIdQuery := SongLastIdQuery;
  FSongDeleteByIdCommand := SongDeleteByIdCommand;
  FSongFactoryFunc := SongFactoryFunc;
end;

procedure TSongRepository.Delete(const Id: Integer);
begin
  try
    if FSongDeleteByIdCommand.Execute(Id) = 0 then
      raise ESongRepository.CreateFmt('Id #%d not found.', [Id]);
  except
    on E: Exception do
      raise ESongRepository.CreateFmt('Song #%d could not be deleted. Error message: %s', [Id, E.Message]);
  end;
end;

function TSongRepository.GetById(const Id: Integer): ISong;
var
  Items: IReadonlyList<ISongRecord>;
begin
  Result := nil;
  try
    Items := FSongByIdQuery.Open(Id);
    if Items.Count <> 1 then
      raise ESongRepository.CreateFmt('%d Songs found for Id #%d.', [Items.Count, Id]);

    Result := FSongFactoryFunc();
    Mappers.Map(Items[0], Result);
  except
    on E: Exception do
      raise ESongRepository.CreateFmt('Song #%d could not be found. Error message: %s', [Id, E.Message]);
  end;
end;

function TSongRepository.GetList: IDeepObservableList<ISong>;
var
  Song: ISong;
  Item: ISongRecord;
begin
  Result := TCollections.GetListOfDeepObservable<ISong>;
  for Item in FSongListQuery.Open do
  begin
    Song := FSongFactoryFunc();
    Mappers.Map(Item, Song);
    Result.Add(Song);
  end;
end;

function TSongRepository.GetNew: ISong;
begin
  Result := FSongFactoryFunc();
end;

function TSongRepository.Store(const Song: ISong): ISong;
begin
  Guard.CheckNotNull(Song, 'Song');
  try
    if FSongInsertCommand.Execute(Song.Title) = 0 then
      raise ESongRepository.CreateFmt('Song "%s" not found.', [Song.Title]);
    Result := GetNew;
    Result.SetId(FSongLastIdQuery.Execute.First.Id);
    Result.SetTitle(Song.Title);
  except
    on E: Exception do
      raise ESongRepository.CreateFmt('Song "%s" could not be stored. Error message: %s', [Song.Title, E.Message]);
  end;
end;

procedure TSongRepository.Update(const Song: ISong);
begin
  Guard.CheckNotNull(Song, 'Song');
  try
    if FSongUpdateByIdCommand.Execute(Song.Id, Song.Title) = 0 then
      raise ESongRepository.CreateFmt('Song #%d not found.', [Song.Id]);
  except
    on E: Exception do
      raise ESongRepository.CreateFmt('Song #%d could not be updated. Error message: %s', [Song.Id, E.Message]);
  end;
end;

end.

