unit Main.ViewModel;

interface

uses
  System.SysUtils,

  Spring,

  Main.ViewModel.Intf,
  Song.View;

type
  TMainViewModel = class(TinterfacedObject, IMainViewModel)
  private
    FSongViewFactoryFunc: TFunc<Integer, TSongView>;
  public
    constructor Create(const SongViewFactoryFunc: TFunc<Integer, TSongView>); reintroduce;

    procedure ShowSong;
  end;

implementation

{ TMainViewModel }

constructor TMainViewModel.Create(const SongViewFactoryFunc: TFunc<Integer, TSongView>);
begin
  Guard.CheckTrue(Assigned(SongViewFactoryFunc), 'SongViewFactoryFunc is not assigned');
  inherited Create;
  FSongViewFactoryFunc := SongViewFactoryFunc;
end;

procedure TMainViewModel.ShowSong;
begin
  FSongViewFactoryFunc(1).Show;
end;

end.
