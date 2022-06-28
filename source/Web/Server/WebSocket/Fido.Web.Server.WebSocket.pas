(*
 * Copyright 2022 Mirko Bianco (email: writetomirko@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

unit Fido.Web.Server.WebSocket;

interface

uses
  System.StrUtils,
  System.SysUtils,
  System.SyncObjs,
  System.Classes,
  System.RegularExpressions,
  System.Generics.Collections,

  IdCustomTCPServer,
  IdHashSHA,
  IdSSLOpenSSL,
  IdContext,
  IdSSL,
  IdIOHandler,
  IdGlobal,
  IdCoderMIME,

  Spring,
  Spring.Collections,

  Fido.Exceptions,
  Fido.Http.Utils,
  Fido.Http.Types,
  Fido.Web.Server.WebSocket.Intf,
  Fido.Web.Server.WebSocket.Client,
  Fido.Web.Server.WebSocket.Clients;

type
  EWebSocketServer = class(EFidoException);

  TWebSocketServer = class(TInterfacedObject, IWebSocketServer)
  private type
    TInternalServerWebSocket = class(TIdCustomTCPServer)
    strict private
      FClients: TClients;
      FHandshakesMap: IDictionary<TIdIOHandler, string>;

      procedure PerformHandShake(const Context: TIdContext);
      procedure HandshakeIfNeeded(const Context: TIdContext);
    protected
      function DoExecute(Context: TIdContext): Boolean; override;
      procedure DoConnect(Context: TIdContext); override;
    public
      constructor Create;
      destructor Destroy; override;

      property Clients: TClients read FClients;
      property HandshakesMap: IDictionary<TIdIOHandler, string> read FHandshakesMap;
      property OnExecute;
    end;

  private var
    FLock: TCriticalSection;
    FEngine: TInternalServerWebSocket;
    FTopics: IDictionary<string, TWebSocketOnReceivedData>;

    procedure OnDisconnect(Context: TIdContext);
    procedure OnExecute(Context: TIdContext);
    function ConvertSSLVersion(const SSLVersion: TSSLVersion): TIdSSLVersion;
    function OnVerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
  public
    constructor Create(const Port: Integer; const SSLCertData: TSSLCertData);
    destructor Destroy; override;

    function Clients: TClients;

    procedure Send(const Topic: string; const Message: string); overload;
    procedure Send(const Topic: string; const Data: TArray<Byte>); overload;

    procedure RegisterTopic(const Topic: string; const OnReceivedData: TWebSocketOnReceivedData = nil);
    function Started: Boolean;
    procedure Start;
    procedure Stop;
  end;

  TWebSocketServer<T> = class(TInterfacedObject, IWebSocketServer<T>)
  private
    FServer: IWebSocketServer;
  public
    constructor Create(const Server: IWebSocketServer); reintroduce;

    procedure Send(const Topic: string; const Item: T); overload;
  end;

implementation

{ TWebSocketServer }

function TWebSocketServer.Clients: TClients;
begin
  Result := FEngine.Clients;
end;

constructor TWebSocketServer.Create(
  const Port: Integer;
  const SSLCertData: TSSLCertData);
begin
  inherited Create;

  FTopics := TCollections.CreateDictionary<string, TWebSocketOnReceivedData>;
  FLock := TCriticalSection.Create;

  FEngine := TInternalServerWebSocket.Create;
  FEngine.OnDisconnect := OnDisconnect;
  FEngine.OnExecute := OnExecute;

  FEngine.DefaultPort := Port;

  if SSLCertData.IsValid then
  begin
    FEngine.IOHandler := TIdServerIOHandlerSSLOpenSSL.Create(nil);
    TIdServerIOHandlerSSLOpenSSL(FEngine.IOHandler).SSLOptions.RootCertFile := SSLCertData.SSLRootCertFilePath;
    TIdServerIOHandlerSSLOpenSSL(FEngine.IOHandler).SSLOptions.CertFile := SSLCertData.SSLCertFilePath;
    TIdServerIOHandlerSSLOpenSSL(FEngine.IOHandler).SSLOptions.KeyFile := SSLCertData.SSLKeyFilePath;
    TIdServerIOHandlerSSLOpenSSL(FEngine.IOHandler).SSLOptions.Method := ConvertSSLVersion(SSLCertData.SSLVersion);
    TIdServerIOHandlerSSLOpenSSL(FEngine.IOHandler).SSLOptions.Mode := sslmUnassigned;
    TIdServerIOHandlerSSLOpenSSL(FEngine.IOHandler).OnGetPassword := nil;
    TIdServerIOHandlerSSLOpenSSL(FEngine.IOHandler).OnVerifyPeer := OnVerifyPeer;
  end;

  FEngine.Active := True;
end;

destructor TWebSocketServer.Destroy;
begin
  Stop;
  FEngine.Free;
  FLock.Free;
  inherited;
end;

function TWebSocketServer.OnVerifyPeer(
  Certificate: TIdX509;
  AOk: Boolean;
  ADepth, AError: Integer): Boolean;
begin
  result := AOk;
end;

function TWebSocketServer.ConvertSSLVersion(const SSLVersion: TSSLVersion): TIdSSLVersion;
begin
  case SSLVersion of
    SSLv2: Result := sslvSSLv2;
    SSLv23: Result := sslvSSLv23;
    SSLv3: Result := sslvSSLv3;
    TLSv1: Result := sslvTLSv1;
    TLSv1_1: Result := sslvTLSv1_1;
    TLSv1_2: Result := sslvTLSv1_2;
  else
    raise EWebSocketServer.Create('SSL version not supported');
  end;
end;

procedure TWebSocketServer.OnDisconnect(Context: TIdContext);
begin
  FEngine.Clients.Remove(Context);
end;

procedure TWebSocketServer.OnExecute(Context: TIdContext);
var
  Client: TWebSocketClient;
  URI: string;
  Params: TArray<TNamedValue>;
  TopicTokens: TArray<string>;
  ParamsTokens: TArray<string>;
  Index: Integer;
  Callback: TWebSocketOnReceivedData;
begin
  if not FEngine.Clients.TryGetByContext(Context, Client) then
    Exit;

  if not FTopics.Keys.TryGetFirst(
     URI,
     function(const Item: string): Boolean
     begin
       Result := TRegEx.IsMatch(Client.Topic.ToUpper, TranslatePathToRegEx(Item.ToUpper)) or
         (Client.Topic.IsEmpty and Item.IsEmpty);
     end) then
    Exit;

  TopicTokens := URI.ToUpper.Split(['/']);
  ParamsTokens := Client.Topic.Split(['/']);

  for Index := Low(TopicTokens) to High(TopicTokens) do
    if TopicTokens[Index].StartsWith('{') and
       TopicTokens[Index].EndsWith('}') then
    begin
      SetLength(Params, Length(Params) + 1);
      Params[High(Params)] := TNamedValue.Create(ParamsTokens[Index], TopicTokens[Index].TrimLeft(['{']).TrimRight(['}']));
    end;

  Callback := FTopics[URI];
  if Assigned(Callback) then
    Callback(Client, Params);
end;

procedure TWebSocketServer.RegisterTopic(
  const Topic: string;
  const OnReceivedData: TWebSocketOnReceivedData);
begin
  FLock.Enter;
  try
    FTopics[Topic] := OnReceivedData;
  finally
    FLock.Leave;
  end;
end;

procedure TWebSocketServer.Send(
  const Topic: string;
  const Message: string);
begin
  FEngine.Clients.ForEachByTopic(Topic, procedure(const Client: TWebSocketClient)
    begin
      Client.Send(Message);
    end);
end;

procedure TWebSocketServer.Send(
  const Topic: string;
  const Data: TArray<Byte>);
begin
  FEngine.Clients.ForEachByTopic(Topic, procedure(const Client: TWebSocketClient)
    begin
      Client.Send(Data);
    end);
end;

procedure TWebSocketServer.Start;
begin
  if not FEngine.Active then
    FEngine.Active := True;
end;

function TWebSocketServer.Started: Boolean;
begin
  Result := FEngine.Active;
end;

procedure TWebSocketServer.Stop;
begin
  if not FEngine.Active then
    Exit;

  FEngine.StopListening;
  FEngine.Active := False;
end;

{ TWebSocketServer.TInternalServerWebSocket }

constructor TWebSocketServer.TInternalServerWebSocket.Create;
begin
  inherited Create(nil);

  FHandshakesMap := TCollections.CreateDictionary<TIdIOHandler, string>([]);

  FClients := TClients.Create;
end;

destructor TWebSocketServer.TInternalServerWebSocket.Destroy;
begin
  FClients.Free;
  inherited;
end;

procedure TWebSocketServer.TInternalServerWebSocket.DoConnect(Context: TIdContext);
begin
  if (Context.Connection.IOHandler is TIdSSLIOHandlerSocketBase) then
    TIdSSLIOHandlerSocketBase(Context.Connection.IOHandler).PassThrough := False;

  inherited;
end;

function TWebSocketServer.TInternalServerWebSocket.DoExecute(Context: TIdContext): Boolean;
begin
  HandshakeIfNeeded(Context);

  Result := inherited;
end;

procedure TWebSocketServer.TInternalServerWebSocket.HandshakeIfNeeded(const Context: TIdContext);
var
  Dummy: string;
begin
  if FHandshakesMap.TryGetValue(Context.Connection.IOHandler, Dummy) then
    Exit;

  PerformHandShake(Context);
end;

procedure TWebSocketServer.TInternalServerWebSocket.PerformHandShake(const Context: TIdContext);
var
  Buffer: string;
  Topic: string;
  Headers: IDictionary<string, string>;
  Hash: Shared<TIdHashSHA1>;
begin
  Context.Connection.IOHandler.CheckForDataOnSource(10);

  try
    Buffer := ParseIOHandlerInputBuffer(Context.Connection.IOHandler);
  except
    on E: Exception do
      raise EWebSocketServer.CreateFmt('Error while performing handshake: %s', [E.Message]);
  end;
  Headers := ParseIOHandlerHeaders(Buffer);
  Topic := ParseIOHandlerTopic(Buffer);

  FHandshakesMap.AddOrSetValue(Context.Connection.IOHandler, Topic);
  FClients.Add(Context, Topic);

  if Headers.ContainsKey('Upgrade') and Headers.ContainsKey('Sec-WebSocket-Key') and
     Headers['Upgrade'].ToLower.Equals('websocket') then
  begin
    try
      Hash := TIdHashSHA1.Create;
      Context.Connection.IOHandler.Write(
        Format(
          'HTTP/1.1 101 Switching Protocols'#13#10 +
          'Upgrade: websocket'#13#10 +
          'Connection: Upgrade'#13#10 +
          'Sec-WebSocket-Accept: %s'#13#10#13#10, [TIdEncoderMIME.EncodeBytes(Hash.Value.HashString(Headers['Sec-WebSocket-Key'] + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'))]),
        IndyTextEncoding_UTF8);
    except
      on E: Exception do
        raise EWebSocketServer.CreateFmt('Error while Switching protocol: %s', [E.Message]);
    end;
  end;
end;

{ TWebSocketServer<T> }

constructor TWebSocketServer<T>.Create(const Server: IWebSocketServer);
begin
  inherited Create;

  FServer := Server;
end;

procedure TWebSocketServer<T>.Send(const Topic: string; const Item: T);
begin
  FServer.Clients.ForEachByTopic(Topic, procedure(const Client: TWebSocketClient)
    begin
      Client.Send<T>(Item);
    end);
end;

end.
