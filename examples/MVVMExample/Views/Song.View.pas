unit Song.View;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.ActnList,
  Vcl.Mask,

  Spring,

  Fido.Gui.Binding.Attributes,
  Fido.Gui.Binding,
  Fido.Gui.Types,

  Song.ViewModel.Intf;

type
  TSongView = class(TForm)
    // The View observes the ViewModel and sets the edTitle.Text to ViewModel.Text
    // everytime the edTitle.OnExit event is called.
    [BidirectionalToObservableBinding('Title', 'Text', 'OnExit')]
    edTitle: TLabeledEdit;
    // When the user presses the button then the ViewModel.Save method is called
    // If the component is linked to an action then that action is triggered before the ViewModel method.
    [MethodToActionBinding('Save', oeetBefore)]
    btnSave: TBitBtn;
    btnCancel: TBitBtn;
    ActionList: TActionList;
    actClose: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actCloseExecute(Sender: TObject);
  private var
    { Private declarations }
    FSongViewModel: ISongViewModel;
  private
    procedure InitializeGui;
  public
    { Public declarations }
    constructor Create(const SongViewModel: ISongViewModel); reintroduce;
  end;

implementation

{$R *.dfm}

{ TSongView }

procedure TSongView.actCloseExecute(Sender: TObject);
begin
  btnSave.SetFocus;
  Close;
end;

constructor TSongView.Create(const SongViewModel: ISongViewModel);
begin
  Guard.CheckNotNull(SongViewModel, 'SongViewModel');
  inherited Create(nil);
  FSongViewModel := SongViewModel;
  InitializeGui;
  FSongViewModel.Broadcast('Initialization');
end;

procedure TSongView.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TSongView.InitializeGui;
begin
  // Sets up the bindings between the ViewModel and the View
  Guibinding.Setup<ISongViewModel, TSongView>(FSongViewModel, Self);
  // Sets up the methods binding between the ViewModel and the View
  Guibinding.MethodsSetup<ISongViewModel, TSongView>(FSongViewModel, Self);
end;

end.
