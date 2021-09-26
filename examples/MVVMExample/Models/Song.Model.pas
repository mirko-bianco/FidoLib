unit Song.Model;

interface

uses
  Spring,

  Fido.Collections.DeepObservableList.Intf,

  Song.DomainObject.Intf,
  Song.Repository.Intf,
  Song.Model.Intf;

type
  TSongModel = class(TInterfacedObject, ISongModel)
  private
    FSongRepository: ISongRepository;
  public
    constructor Create(const SongRepository: ISongRepository);

    function GetList: IDeepObservableList<ISong>;

    function New: ISong;

    function Get(const Id: Integer): ISong;

    procedure Save(const Song: ISong);
  end;

implementation

{ TSongModel }

constructor TSongModel.Create(const SongRepository: ISongRepository);
begin
  Guard.CheckNotNull(SongRepository, 'SongRepository');

  inherited Create;
  FSongRepository := SongRepository;
end;

function TSongModel.Get(const Id: Integer): ISong;
begin
  Result := FSongRepository.GetById(Id);
end;

function TSongModel.GetList: IDeepObservableList<ISong>;
begin
  Result := FSongRepository.GetList;
end;

function TSongModel.New: ISong;
begin
  Result := FSongRepository.GetNew;
end;

procedure TSongModel.Save(const Song: ISong);
begin
  FSongRepository.Update(Song);
end;

end.
