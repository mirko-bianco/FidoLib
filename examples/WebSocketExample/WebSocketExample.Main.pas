unit WebSocketExample.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ShellAPI,

  System.SysUtils, System.Variants, System.Classes, System.NetEncoding,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls,

  Spring.Collections,

  Fido.http.Types,
  Fido.http.Response.Intf,
  Fido.http.Response,
  Fido.http.Request.Intf,
  Fido.http.Request,
  Fido.Api.Server.Intf,
  Fido.Api.Server.Indy,
  Fido.Api.Server.Resource.Attributes,

  Fido.Web.Server.Files,
  Fido.Web.Server.WebSocket.Loop.Intf,
  Fido.Web.Server.WebSocket.Loop.Abstract,
  Fido.Web.Server.WebSocket.Tool;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
  private
    FRestServer: IApiServer;

  public
    procedure AfterConstruction; override;
  end;

  [WebSocketPath('/')]
  TMyWebSocket = class(TLoopServerWebSocket)
  protected
    procedure OnReceivedData(const Data: TBytes); override;
    procedure OnReceivedMessage(const Str: string); override;
    procedure Step; override;
    function GetLoopDuration: Integer; override;
  public
    constructor Create(
      const Key: string;
      const HttpResponse: IHttpResponse);

    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AfterConstruction;
begin
  inherited;
  FRestServer := TIndyApiServer.Create(
    80,
    1000,
    TFileWebServer.Create(
      '.',
      'index.html'),
    TSSLCertData.CreateEmpty);
  FRestServer.RegisterWebSocket(TMyWebSocket);
  FRestServer.SetActive(True);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://127.0.0.1', nil, nil, SW_SHOW);
end;

{ TMyWebSocket }

constructor TMyWebSocket.Create(
  const Key: string;
  const HttpResponse: IHttpResponse);
begin
  inherited Create(Key, HttpResponse);
end;

destructor TMyWebSocket.Destroy;
begin
  inherited;
end;

function TMyWebSocket.GetLoopDuration: Integer;
begin
  Result := 1000;
end;

procedure TMyWebSocket.OnReceivedData(const Data: TBytes);
begin
  inherited;

end;

procedure TMyWebSocket.OnReceivedMessage(const Str: string);
begin
  inherited;
  TThread.Synchronize(nil, procedure
  begin
    Form1.Memo1.Lines.Add('<< ' + Str);
  end);
end;

procedure TMyWebSocket.Step;
begin
  SendMessage(DateTimeToStr(Now));
end;

end.
