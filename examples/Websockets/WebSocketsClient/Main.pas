unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,

  Fido.Web.Client.WebSocket.Intf,
  Fido.Web.Client.WebSocket;

type
  TForm1 = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    edtMessage: TEdit;
    btnSend: TButton;
    mmMessages: TMemo;
    procedure btnStartClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
  private
    { Private declarations }
    FClient: IWebSocketClient;
  public
    { Public declarations }
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AfterConstruction;
begin
  inherited;
  FClient := TWebSocketClient.Create('ws://127.0.0.1:8080/atopic/100');
  BtnSend.Enabled := False;
  btnStop.Enabled := False;
end;

procedure TForm1.BeforeDestruction;
begin
  FClient.Stop;
  inherited;
end;

procedure TForm1.btnSendClick(Sender: TObject);
begin
  FClient.Send(edtMessage.Text);
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  FClient.Start(procedure(const Message: string)
    begin
      TThread.Synchronize(nil, procedure
        begin
          mmMessages.Lines.Add(Message);
        end);
    end);

  BtnSend.Enabled := True;
  btnStart.Enabled := False;
  btnStop.Enabled := True;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  BtnSend.Enabled := False;
  btnStart.Enabled := True;
  btnStop.Enabled := False;
  FClient.Stop;
end;

end.
