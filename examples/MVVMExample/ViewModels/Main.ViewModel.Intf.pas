unit Main.ViewModel.Intf;

interface

uses
  Data.DB,
  Vcl.DBGrids,

  Fido.DesignPatterns.Observable.Intf;

type
  IMainViewModel = interface(IObservable)
  ['{0AAE15DB-F2A5-4155-AA90-F179677CF482}']

    function GetSongDataset: TDataSet;
    procedure ShowSong;
    procedure NewSong;
    procedure DeleteSong;

    function IsModelNotEmpty: Boolean;
    function SongsGridTitleClick: TDBGridClickEvent;
  end;

implementation

end.
