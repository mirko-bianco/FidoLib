unit Main.View;

interface

uses
  System.Classes,
  Vcl.Forms,
  Vcl.StdCtrls,

  Spring,

  Fido.Gui.Binding.Attributes,
  Fido.Gui.Binding,
  Fido.Gui.Types,

  Main.ViewModel.Intf, Vcl.Controls;

type
  TMainView = class(TForm)
    [MethodToActionBinding('ShowSong', oeetAfter)]
    Button1: TButton;
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
  Guibinding.MethodsSetup<IMainViewModel, TMainView>(FMainViewModel, Self);
end;

end.
