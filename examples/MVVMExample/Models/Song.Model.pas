unit Song.Model;

interface

uses
  System.SysUtils,

  Spring,
  Spring.Collections,

  Fido.Utilities,
  Fido.Exceptions,
  Fido.Collections.DeepObservableList.Intf,

  Song.DomainObject.Intf,
  Song.Repository.Intf,
  Song.Model.Intf;

type
  ESongModel = class(EFidoException);

  TSongModel = class(TInterfacedObject, ISongModel)
  private
    FSongRepository: ISongRepository;
    FList: IDeepObservableList<ISong>;
  public
    constructor Create(const SongRepository: ISongRepository);

    function GetList: IDeepObservableList<ISong>;

    function New: ISong;

    function Get(const Id: Integer): ISong;

    procedure Save(const Song: ISong);

    procedure Delete(const Id: Integer);
  end;

implementation

{ TSongModel }

constructor TSongModel.Create(const SongRepository: ISongRepository);
begin
  inherited Create;
  FSongRepository := Utilities.CheckNotNullAndSet(SongRepository, 'SongRepository');
end;

procedure TSongModel.Delete(const Id: Integer);
var
  Song: ISong;
begin
  FSongRepository.Delete(Id);

  if not Assigned(FList) then
    Exit;

  Song := FList.First(
    function(const Item: ISong): Boolean
    begin
      Result := Item.Id = Id;
    end);

  if not Assigned(Song) then
    Exit;

  FList.Remove(Song);
end;

function TSongModel.Get(const Id: Integer): ISong;
begin
  Result := FSongRepository.GetById(Id);
end;

function TSongModel.GetList: IDeepObservableList<ISong>;
begin
  if not Assigned(FList) then
    FList := FSongRepository.GetList;
  Result := FList;
end;

function TSongModel.New: ISong;
begin
  Result := FSongRepository.GetNew;
end;

procedure TSongModel.Save(const Song: ISong);
var
  LSong: ISong;
begin
  Guard.CheckNotNull(Song, 'Song');

  if Song.Title.IsEmpty then
    raise ESongModel.Create('Validation error: Song.Title cannot be empty.');

  if Song.Id < 1 then
  begin
    FList.Add(FSongRepository.Store(Song));
    Exit;
  end;

  FSongRepository.Update(Song);

  if not Assigned(FList) then
    Exit;

  LSong := FList.First(
    function(const Item: ISong): Boolean
    begin
      Result := Item.Id = Song.Id;
    end);

  if not Assigned(LSong) then
  Exit;

  LSong.SetTitle(Song.Title);
end;

end.
