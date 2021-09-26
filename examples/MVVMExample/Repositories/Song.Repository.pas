unit Song.Repository;

interface

uses
  System.SysUtils,

  Spring,
  Spring.Collections,

  Fido.Collections.DeepObservableList.Intf,
  Fido.Collections,
  Fido.Mappers,
  Fido.VirtualQuery,

  Song.Repository.Intf,
  Song.Repository.Data.Intf,
  Song.DomainObject.Intf;

type
  TSongRepository = class(TInterfacedObject, ISongRepository)
  private
    FSongListQuery: ISongListQuery;
    FSongByIdQuery: ISongByIdQuery;
    FSongUpdateByIdCommand: ISongUpdateByIdCommand;
    FSongFactoryFunc: TFunc<ISong>;
  public
    constructor Create(
      const SongListQuery: ISongListQuery;
      const SongByIdQuery: ISongByIdQuery;
      const SongUpdateByIdCommand: ISongUpdateByIdCommand;
      const SongFactoryFunc: TFunc<ISong>);

    function GetNew: ISong;
    function GetList: IDeepObservableList<ISong>;
    function GetById(const Id: Integer): ISong;
    procedure Update(const Song: ISong);
  end;

implementation

{ TSongRepository }

constructor TSongRepository.Create(
  const SongListQuery: ISongListQuery;
  const SongByIdQuery: ISongByIdQuery;
  const SongUpdateByIdCommand: ISongUpdateByIdCommand;
  const SongFactoryFunc: TFunc<ISong>);
begin
  Guard.CheckNotNull(SongListQuery, 'SongListQuery');
  Guard.CheckNotNull(SongByIdQuery, 'SongListById');
  Guard.CheckNotNull(SongUpdateByIdCommand, 'SongUpdateByIdCommand');
  Guard.CheckTrue(Assigned(SongFactoryFunc), 'FSongFactoryFunc is not assigned');
  inherited Create;
  FSongListQuery := SongListQuery;
  FSongByIdQuery := SongByIdQuery;
  FSongUpdateByIdCommand := SongUpdateByIdCommand;
  FSongFactoryFunc := SongFactoryFunc;
end;

function TSongRepository.GetById(const Id: Integer): ISong;
var
  Items: IReadonlyList<ISongRecord>;
begin
  Result := nil;
  Items := FSongByIdQuery.Open(Id);
  if Items.Count = 1 then
  begin
    Result := FSongFactoryFunc();
    Mappers.Map(Items[0], Result);
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

procedure TSongRepository.Update(const Song: ISong);
begin
  Guard.CheckNotNull(Song, 'Song');
  FSongUpdateByIdCommand.Update(Song.Id, Song.Title);
end;

end.

