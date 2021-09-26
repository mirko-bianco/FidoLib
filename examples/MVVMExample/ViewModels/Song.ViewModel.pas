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
  public
    constructor Create(
      const Id: Integer;
      const SongModel: ISongModel);

    function GetTitle: string;
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
  Song := SongModel.Get(Id);
  inherited Create(Song); //This ensure that the ViewModel is the delegate observable for the domain object
  FSong := Song;
  FSongModel := SongModel;
end;


function TSongViewModel.GetTitle: string;
begin
  Result := FSong.Title;
end;

procedure TSongViewModel.Save;
begin
  FSongModel.Save(FSong);
end;

procedure TSongViewModel.SetTitle(const Title: string);
begin
  FSong.Title := Title;
end;

end.
