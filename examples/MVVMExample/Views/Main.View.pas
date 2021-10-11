unit Main.View;

interface

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Actions,
  Data.DB,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.Dialogs,
  Vcl.ActnList,

  Spring,

  Fido.Gui.Binding.Attributes,
  Fido.Gui.Binding,
  Fido.Gui.Types,

  Main.ViewModel.Intf;

type
  TMainView = class(TForm)
    // When the user presses the button then the ViewModel.ShowSong method is called
    // If the component is linked to an action then that action is triggered before the ViewModel method.
    [MethodToActionBinding('ShowSong', oeetBefore)]
    // The View observes the ViewModel and sets the Button.Enabled to ViewModel.IsModelNotEmpty
    // everytime there is a notification.
    [UnidirectionalToGuiBinding('IsModelNotEmpty',  'Enabled')]
    ShowButton: TButton;
    SongsDataSource: TDataSource;
    // The SongsGrid.OnTitleClick is bound to the ViewModel.SongsGridTitleClick method that returns the exact
    // type for the event (TDBGridClickEvent).
    [BindEvent('SongsGridTitleClick', 'OnTitleClick')]
    SongsGrid: TDBGrid;
    // When the user presses the button then the ViewModel.NewSong method is called
    // If the component is linked to an action then that action is triggered before the ViewModel method.
    [MethodToActionBinding('NewSong', oeetBefore)]
    NewButton: TButton;
    ActionList: TActionList;
    actNewSong: TAction;
    // When the user presses the button then the ViewModel.DeleteSong method is called
    // If the component is linked to an action then that action is triggered before the ViewModel method.
    [MethodToActionBinding('DeleteSong', oeetBefore)]
    // The View observes the ViewModel and sets the Button.Enabled to ViewModel.IsModelNotEmpty
    // everytime there is a notification.
    [UnidirectionalToGuiBinding('IsModelNotEmpty',  'Enabled')]
    DeleteButton: TButton;
    actDeleteSong: TAction;
    procedure SongsGridDblClick(Sender: TObject);
    procedure actDeleteSongExecute(Sender: TObject);
  private
    { Private declarations }
    FMainViewModel: IMainViewModel;

    procedure InitializeGui;
  public
    { Public declarations }
    constructor Create(
      const Owner: TComponent;
      const MainViewModel: IMainViewModel); reintroduce;
  end;

implementation

{$R *.dfm}

{ TMainView }

procedure TMainView.actDeleteSongExecute(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to delete the song?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    // The abort will interrupt the UI flow and stop the decorated event from happening.
    // This way the ViewModel can deal with the pure logic while the View can be in charge of the UI interactions.
    Abort;
end;

constructor TMainView.Create(
  const Owner: TComponent;
  const MainViewModel: IMainViewModel);
begin
  inherited Create(Owner);
  Guard.CheckNotNull(MainViewModel, 'MainViewModel');
  FMainViewModel := MainViewModel;
  InitializeGui;
end;

procedure TMainView.InitializeGui;
begin
  // Sets up the bindings between the ViewModel and the View
  Guibinding.Setup<IMainViewModel, TMainView>(FMainViewModel, Self);
  // Sets up the methods binding between the ViewModel and the View
  Guibinding.MethodsSetup<IMainViewModel, TMainView>(FMainViewModel, Self);

  SongsDataSource.DataSet := FMainViewModel.GetSongDataset;
  ShowButton.Enabled := FMainViewModel.IsModelNotEmpty;
  DeleteButton.Enabled := FMainViewModel.IsModelNotEmpty;
end;

procedure TMainView.SongsGridDblClick(Sender: TObject);
begin
  // Double clicking on the grid will show the song, but only if the button is enabled.
  if ShowButton.Enabled then
    ShowButton.Click;
end;

end.
