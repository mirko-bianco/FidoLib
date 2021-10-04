unit Song.ViewModel;

interface

uses
  SysUtils,

  Spring,

  Fido.DesignPatterns.Observable.Delegated,

  Song.DomainObject.Intf,
  Song.ViewModel.Intf,
  Song.Model.Intf;

type
  TSongViewModel = class(TDelegatedObservable, ISongViewModel)
  private
    FSongModel: ISongModel;
    FSong: ISong;

    function RetrieveSong(const SongModel: ISongModel; const Id: Integer): ISong;
  public
    constructor Create(
      const Id: Integer;
      const SongModel: ISongModel);

    function Title: string;
    procedure SetTitle(const Title: string);

    procedure Save;
  end;

implementation

{ TSongViewModel }

constructor TSongViewModel.Create(
  const Id: Integer;
  const SongModel: ISongModel);
var
  Song: ISong;
begin
  Guard.CheckNotNull(SongModel, 'SongModel');
  Song := RetrieveSong(SongModel, Id);
  //This ensure that the ViewModel is the delegate observable for the domain object
  inherited Create(Song);
  FSong := Song;
  FSongModel := SongModel;
end;


function TSongViewModel.Title: string;
begin
  Result := FSong.Title;
end;

function TSongViewModel.RetrieveSong(const SongModel: ISongModel; const Id: Integer): ISong;
begin
  if Id > 0 then
    Result := SongModel.Get(Id)
  else
    Result := SongModel.New;
end;

procedure TSongViewModel.Save;
begin
  FSongModel.Save(FSong);
end;

procedure TSongViewModel.SetTitle(const Title: string);
begin
  FSong.SetTitle(Title);
end;

end.
