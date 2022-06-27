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

  Spring,

  Fido.Http.Types,
  Fido.Web.Server.WebSocket.Intf,
  Fido.Web.Server.WebSocket,
  Fido.Web.Server.WebSocket.Client;

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
    FServer: IWebSocketServer;
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
  FServer := TWebSocketServer.Create(8080, TSSLCertData.CreateEmpty);
  FServer.RegisterTopic(
    'atopic/{id}',
    procedure(const Client: TWebSocketClient; const Params: array of TNamedValue)
    var
      Message: string;
      LParams: string;
    begin
      LParams := Params[0].Name + '=' + Params[0].Value.ToString;
      Message := Client.WaitForMessage;
      if Message.IsEmpty then
        Exit;
      TThread.Synchronize(nil, procedure
        begin
          mmMessages.Lines.Add(Client.Topic + ' ' + LParams + ' ' + Client.Host + ': ' + Message);
        end);
    end);

  BtnSend.Enabled := False;
  btnStop.Enabled := False;
end;

procedure TForm1.BeforeDestruction;
begin
  FServer.Stop;
  inherited;
end;

procedure TForm1.btnSendClick(Sender: TObject);
begin
  FServer.Send('atopic/100', edtMessage.Text);
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  FServer.Start;

  BtnSend.Enabled := True;
  btnStart.Enabled := False;
  btnStop.Enabled := True;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  BtnSend.Enabled := False;
  btnStart.Enabled := True;
  btnStop.Enabled := False;
  FServer.Stop;
end;

end.
