unit Main.ViewModel;

interface

uses
  System.SysUtils,
  Data.DB,
  VCL.Dialogs,
  Vcl.DBGrids,

  Spring.Collections,

  Fido.Utilities,
  Fido.Db.ListDatasets,
  Fido.DesignPatterns.Observer.Intf,
  Fido.DesignPatterns.Observer.Notification.Intf,
  Fido.DesignPatterns.Observable.Intf,
  Fido.DesignPatterns.Observable.Delegated,
  Fido.Collections.DeepObservableList.Intf,
  Fido.Db.ListDataset,

  Song.DomainObject.Intf,
  Song.Model.Intf,
  Main.ViewModel.Intf,
  Song.ViewModel.Intf,
  Song.View;

type
  // The MainViewModel is observed by the View and observes and relays the notifications
  // from the list
  TMainViewModel = class(TDelegatedObservable, IMainViewModel, IObserver)
  private
    FSongModel: ISongModel;
    FSongViewFactoryFunc: TFunc<ISongViewModel, TSongView>;
    FSongViewModelFactoryFunc: TFunc<Integer, ISongModel, ISongViewModel>;

    FList: IDeepObservableList<ISong>;
    FDataSet: TListDataSetOfObservable<ISong>;
  protected
    procedure Notify(const Sender: IInterface; const Notification: INotification);
    procedure DoSongsGridTitleClick(Column: TColumn);
  public
    constructor Create(
      const SongModel: ISongModel;
      const SongViewModelFactoryFunc: TFunc<Integer, ISongModel, ISongViewModel>;
      const SongViewFactoryFunc: TFunc<ISongViewModel, TSongView>); reintroduce;
    destructor Destroy; override;

    function GetSongDataset: TDataSet;
    procedure ShowSong;
    procedure NewSong;
    procedure DeleteSong;
    function IsModelNotEmpty: Boolean;

    function SongsGridTitleClick: TDBGridClickEvent;
  end;

implementation

{ TMainViewModel }

constructor TMainViewModel.Create(
  const SongModel: ISongModel;
  const SongViewModelFactoryFunc: TFunc<Integer, ISongModel, ISongViewModel>;
  const SongViewFactoryFunc: TFunc<ISongViewModel, TSongView>);
var
  ObservableIntf: IObservable;
begin
  inherited Create(nil);
  FSongModel := Utilities.CheckNotNullAndSet(SongModel, 'Model');
  FSongViewModelFactoryFunc := Utilities.CheckNotNullAndSet<TFunc<Integer, ISongModel, ISongViewModel>>(SongViewModelFactoryFunc, 'SongViewModelFactoryFunc');
  FSongViewFactoryFunc := Utilities.CheckNotNullAndSet<TFunc<ISongViewModel, TSongView>>(SongViewFactoryFunc, 'SongViewFactoryFunc');
  FList := SongModel.GetList;
  if Supports(FList, IObservable, ObservableIntf) then
    // Starts observing the list for changes
    ObservableIntf.RegisterObserver(Self);

  FDataSet := ListDatasets.ListOfObservablesToDataset<ISong>(FList);
  Broadcast('Initilialized');
end;

procedure TMainViewModel.DeleteSong;
begin
  FSongModel.Delete(FDataSet.CurrentEntity.Id);
end;

destructor TMainViewModel.Destroy;
begin
  (FList as IObservable).UnregisterObserver(Self);
  FDataset.Free;
  inherited;
end;

procedure TMainViewModel.DoSongsGridTitleClick(Column: TColumn);
begin
  ShowMessage(Format('You clicked the "%s" column', [Column.Title.Caption]));
end;

function TMainViewModel.GetSongDataset: TDataSet;
begin
  Result := FDataSet;
end;

function TMainViewModel.IsModelNotEmpty: Boolean;
begin
  Result := false;
  if Assigned(FDataSet) then
    Result := FDataSet.RecordCount > 0;
end;

procedure TMainViewModel.NewSong;
begin
  FSongViewFactoryFunc(FSongViewModelFactoryFunc(-1, FSongModel)).Show;
end;

procedure TMainViewModel.Notify(const Sender: IInterface; const Notification: INotification);
begin
  // Notifies the View when the list has changed.
  Broadcast(Notification);
end;

procedure TMainViewModel.ShowSong;
begin
  FSongViewFactoryFunc(FSongViewModelFactoryFunc(FDataSet.CurrentEntity.Id, FSongModel)).Show;
end;

function TMainViewModel.SongsGridTitleClick: TDBGridClickEvent;
begin
  Result := DoSongsGridTitleClick;
end;

end.
