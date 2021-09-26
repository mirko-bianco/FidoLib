unit Song.View;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons, System.Actions, Vcl.ActnList,

  Spring,

  Fido.Gui.Binding.Attributes,
  Fido.Gui.Binding,
  Fido.Gui.Types,

  Song.ViewModel.Intf;

type
  TSongView = class(TForm)
    [BidirectionalToObservableBinding('Title', 'Text', 'OnExit')]
    edTitle: TLabeledEdit;
    [MethodToActionBinding('Save', oeetAfter)]
    btnOk: TBitBtn;
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
    constructor Create(
      const Id: Integer;
      const SongViewModelFactoryFunc: TFunc<Integer, ISongViewModel>); reintroduce;
  end;

implementation

{$R *.dfm}

{ TSongView }

procedure TSongView.actCloseExecute(Sender: TObject);
begin
  Close;
end;

constructor TSongView.Create(
  const Id: Integer;
  const SongViewModelFactoryFunc: TFunc<Integer, ISongViewModel>);
begin
  Guard.CheckTrue(Assigned(SongViewModelFactoryFunc), 'SongViewModelFactoryFunc is not assigned');
  inherited Create(nil);
  FSongViewModel := SongViewModelFactoryFunc(Id);
  InitializeGui;
  FSongViewModel.Broadcast('Initialization');
end;

procedure TSongView.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TSongView.InitializeGui;
begin
  Guibinding.Setup<ISongViewModel, TSongView>(FSongViewModel, Self);
  Guibinding.MethodsSetup<ISongViewModel, TSongView>(FSongViewModel, Self);
end;

end.
