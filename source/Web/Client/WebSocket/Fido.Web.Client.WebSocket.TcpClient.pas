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

unit Fido.Web.Client.WebSocket.TcpClient;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.SyncObjs,
  System.Math,
  System.DateUtils,

  idIOHandler,
  IdHTTP,
  IdURI,
  IdGlobal,
  IdTCPClient,
  IdSSLOpenSSL,
  IdCoderMIME,
  IdHashSHA,
  IdSSLOpenSSLHeaders,

  Spring,

  Fido.Exceptions,
  Fido.Utilities,
  Fido.JSON.Marshalling,
  Fido.Web.Client.WebSocket.TcpClient.Intf,
  Fido.Web.Client.WebSocket.Intf;

type
  TWebSocketIdTCPClient = class(TIdTCPClient)
  public
    destructor Destroy; override;

    function Connected: Boolean; override;
  end;

  TWebSocketTCPClient = class(TinterfacedObject, IWebSocketTCPClient)
  strict private
    FURL: string;
    FSSLVersion: TIdSSLVersion;
    FOnReceivedData: TWebSocketOnReceivedData;
    FOnError: TWebSocketOnError;
    FWebsocketProtocol: string;
    FLock: TCriticalSection;
    FIncomingDataTask: ITask;
    FCloseHandshakeSent: Boolean;
    FUpgraded: Boolean;
    FKey: string;
    FExpectedAcceptResponse: string;
    FClosing: Boolean;

    FInternalTCPClient: TWebSocketIdTCPClient;
  private
    procedure SendCloseHandshake;
    function ClearBit(const Value: Cardinal; const Byte: Byte): Cardinal;
    function GetBit(const Value: Cardinal; const Byte: Byte): Boolean;
    function SetBit(const Value: Cardinal; const Byte: Byte): Cardinal;
    function EncodeFrame(const Data: TIdBytes; const Operation: Byte): TIdBytes;
    procedure StartIncomingThread;
    procedure ValidateWebSocket;
    procedure ValidateHeaders(const Headers: TStrings);
    procedure HandleException(const Exception: Exception);
    function FormatURL(const URL: string): string;
    procedure CheckSsl(const URL: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetOnReceiveData(const OnReceivedData: TWebSocketOnReceivedData);
    procedure SetOnError(const OnError: TWebSocketOnError);

    procedure Connect(const URL: string; const CustomHeaders: TStrings; const WebsocketProtocol: string);
    function Connected: Boolean;
    procedure Send(const Message: string); overload;
    procedure Send(const Data: TArray<Byte>); overload;

    function Get(const URL: string; const CustomHeaders: TStrings): string;
    procedure Close;
  end;

implementation

{ TWebSocketTCPClient }

function TWebSocketTCPClient.ClearBit(
  const Value: Cardinal;
  const Byte: Byte): Cardinal;
begin
  Result := Value and not (1 shl Byte);
end;

procedure TWebSocketTCPClient.Close;
begin
  if not Connected then
    Exit;
  FLock.Enter;
  try
    SendCloseHandshake;
    if Assigned(FInternalTCPClient.IOHandler) then
    begin
      FInternalTCPClient.IOHandler.InputBuffer.Clear;
      FInternalTCPClient.IOHandler.CloseGracefully;
    end;
    FInternalTCPClient.Disconnect;
  finally
    FLock.Leave;
  end;
end;

function TWebSocketTCPClient.Get(
  const URL: string;
  const CustomHeaders: TStrings): string;
var
  Http: IShared<TIdHTTP>;
  FormattedURL: string;
begin
  Http := Shared.Make(TIdHTTP.Create(nil));

  FormattedURL := FormatURL(URL);

  CheckSsl(FormattedURL);

  Http.IOHandler := FInternalTCPClient.Socket;
  Http.Request.UserAgent := 'FidoLib websocket client';
  Http.Request.CustomHeaders.AddStrings(CustomHeaders);

  Result := Http.Get(FormattedURL);
end;

procedure TWebSocketTCPClient.Connect(
  const URL: string;
  const CustomHeaders: TStrings;
  const WebsocketProtocol: string);
var
  URI: IShared<TIdURI>;
  FormattedURL: string;
  Header: string;
begin
  FormattedURL := FormatURL(URL);
  CheckSsl(FormattedURL);

  FWebsocketProtocol := WebsocketProtocol;
  Guard.CheckNotNull(CustomHeaders, 'CustomHeaders');
  if Connected then
    raise EWebSocketTCPClient.Create('Connection already opened.');

  FInternalTCPClient.Connect;


  URI := Shared.Make(TIdURI.Create(FormattedURL));
  if URI.Protocol.ToLower.Contains('wss') then
    URI.Protocol := ReplaceOnlyFirst(URI.Protocol.ToLower, 'wss', 'https')
  else
    URI.Protocol := ReplaceOnlyFirst(URI.Protocol.ToLower, 'ws', 'http');
  if URI.Path.Trim.IsEmpty then
    URI.Path := '/';
  if not URI.Port.IsEmpty then
    URI.Host := URI.Host + ':' + URI.Port;
  if (URI.Params <> '') then
    FInternalTCPClient.IOHandler.WriteLn(Format('GET %s HTTP/1.1', [URI.Path + URI.Document + '?' + URI.Params]))
  else
    FInternalTCPClient.IOHandler.WriteLn(Format('GET %s HTTP/1.1', [URI.Path + URI.Document]));
  FInternalTCPClient.IOHandler.WriteLn(Format('Host: %s', [URI.Host]));
  FInternalTCPClient.IOHandler.WriteLn('User-Agent: FidoLib websocket client');
  FInternalTCPClient.IOHandler.WriteLn('Connection: keep-alive, Upgrade');
  FInternalTCPClient.IOHandler.WriteLn('Upgrade: WebSocket');
  FInternalTCPClient.IOHandler.WriteLn('Sec-WebSocket-Version: 13');
  FInternalTCPClient.IOHandler.WriteLn(Format('Sec-WebSocket-Key: %s', [FKey]));
  if not FWebsocketProtocol.Trim.IsEmpty then
    FInternalTCPClient.IOHandler.WriteLn(Format('Sec-WebSocket-Protocol: %s', [FWebsocketProtocol]));
  for Header in CustomHeaders do
    FInternalTCPClient.IOHandler.WriteLn(Header);
  FInternalTCPClient.IOHandler.WriteLn(EmptyStr);

  FCloseHandshakeSent := False;

  StartIncomingThread;
end;

function TWebSocketTCPClient.Connected: Boolean;
begin
  Result := FInternalTCPClient.Connected;
end;

constructor TWebSocketTCPClient.Create;
var
  Bytes: TIdBytes;
  Index: Integer;
  Hash: IShared<TIdHashSHA1>;
begin
  inherited Create;
  FSSLVersion := sslvTLSv1_2;

  FInternalTCPClient := TWebSocketIdTCPClient.Create(nil);
  FClosing := False;
  FLock := TCriticalSection.Create;

  Randomize;

  SetLength(Bytes, 16);
  for Index := Low(Bytes) to High(Bytes) do
    Bytes[Index] := byte(random(255));
  FKey := TIdEncoderMIME.EncodeBytes(Bytes);

  Hash := Shared.Make(TIdHashSHA1.Create);
  FExpectedAcceptResponse := TIdEncoderMIME.EncodeBytes(Hash.HashString(FKey + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));
end;

destructor TWebSocketTCPClient.Destroy;
begin
  FClosing := True;
  if FIncomingDataTask <> nil then
    FIncomingDataTask.Wait;
  FIncomingDataTask := nil;
  FLock.Free;

  FInternalTCPClient.Free;
  inherited;
end;

function TWebSocketTCPClient.EncodeFrame(
  const Data: TIdBytes;
  const Operation: Byte): TIdBytes;
var
  Fin: Cardinal;
  DataLength: Integer;
  Mask: Cardinal;
  ExtendedPayloadLength: Integer;
  ExtendedPayloads: array[0..3] of Cardinal;
  MaskingKey: array[0..3] of cardinal;
  Buffer: TIdBytes;
  Index: Integer;
  FirstXor: Char;
  SecondXor: Char;
begin
  Fin := 0;
  DataLength := Length(Data);
  Fin := SetBit(Fin, 7) or Operation;
  Mask := SetBit(0, 7);
  ExtendedPayloadLength := 0;
  if (Length(Data) <= 125) then
    Mask := Mask + Cardinal(DataLength)
  else if (DataLength.ToSingle < IntPower(2, 16)) then
  begin
    Mask := Mask + 126;
    ExtendedPayloadLength := 2;
    ExtendedPayloads[1] := Byte(DataLength);
    ExtendedPayloads[0] := Byte(DataLength shr 8);
  end
  else
  begin
    Mask := Mask + 127;
    ExtendedPayloadLength := 4;
    ExtendedPayloads[3] := Byte(DataLength);
    ExtendedPayloads[2] := Byte(DataLength shr 8);
    ExtendedPayloads[1] := Byte(DataLength shr 16);
    ExtendedPayloads[0] := Byte(DataLength shr 32);
  end;
  MaskingKey[0] := Random(255);
  MaskingKey[1] := Random(255);
  MaskingKey[2] := Random(255);
  MaskingKey[3] := Random(255);
  SetLength(Buffer, 6 + ExtendedPayloadLength + DataLength);
  Buffer[0] := Fin;
  Buffer[1] := Mask;
  for Index := 0 to Pred(ExtendedPayloadLength) do
    Buffer[2 + Index] := ExtendedPayloads[Index];
  for Index := 0 to 3 do
    Buffer[2 + ExtendedPayloadLength + Index] := MaskingKey[Index];
  for Index := 0 to Pred(DataLength) do
  begin
    FirstXor := Char(Data[Index]);
    SecondXor := Chr(MaskingKey[((Index) mod 4)]);
    SecondXor := Chr(ord(FirstXor) xor ord(SecondXor));
    Buffer[6 + ExtendedPayloadLength + Index] := Ord(SecondXor);
  end;
  Result := Buffer;
end;

function TWebSocketTCPClient.GetBit(
  const Value: Cardinal;
  const Byte: Byte): Boolean;
begin
  Result := (Value and (1 shl Byte)) <> 0;
end;

procedure TWebSocketTCPClient.HandleException(const Exception: Exception);
var
  Disconnect: Boolean;
begin
  Disconnect := true;
  if Assigned(FOnError) then
    FOnError(Exception, Disconnect);
  if Disconnect then
    Self.Close;
end;

procedure TWebSocketTCPClient.ValidateHeaders(const Headers: TStrings);
begin
  Headers.NameValueSeparator := ':';
  if (Headers.Count = 0) then
    raise EWebSocketTCPClient.Create('No headers found');
  if (not Headers[0].Contains('HTTP/1.1 101')) and
     (Headers[0].Contains('HTTP/1.1')) then
    raise EWebSocketTCPClient.Create(Headers[0].Substring(9));
  if Headers.Values['Connection'].Trim.ToLower.Equals('upgrade') and
     Headers.Values['Upgrade'].Trim.ToLower.Equals('websocket') then
  begin
    if Headers.Values['Sec-WebSocket-Accept'].Trim.Equals(FExpectedAcceptResponse) then
      Exit;
    if Headers.Values['Sec-WebSocket-Accept'].Trim.IsEmpty then
      Exit;
    raise EWebSocketTCPClient.Create('Could not perform handshake');
  end;
end;

procedure TWebSocketTCPClient.ValidateWebSocket;
var
  StringStream: string;
  Data: Byte;
  Headers: IShared<TStringlist>;
begin
  StringStream := EmptyStr;
  Headers := Shared.Make(TStringList.Create);

  FUpgraded := False;
  while Connected and not FUpgraded do
  begin
    Data := FInternalTCPClient.Socket.ReadByte;
    StringStream := StringStream + Chr(Data);
    if (not FUpgraded) and (Data = Ord(#13)) then
    begin
      if (StringStream = #10#13) then
      begin
        ValidateHeaders(Headers);
        FUpgraded := True;
        StringStream := EmptyStr;
      end
      else
      begin
        Headers.Add(StringStream.Trim);
        StringStream := EmptyStr;
      end;
    end;
  end;
end;

procedure TWebSocketTCPClient.StartIncomingThread;
var
  Operation: Byte;
  ByteStream: TIdBytes;
begin
  ValidateWebSocket;
  if not Connected then
    Exit;
  FIncomingDataTask := TTask.Run(
    procedure
    var
      Data: Byte;
      Position: Integer;
      InFrame: Boolean;
      Masked: Boolean;
      Size: Int64;
    begin
      SetLength(ByteStream, 0);
      Position := 0;
      Size := 0;
      Operation := 0;
      InFrame := False;
      try
        while Connected and not FClosing do
        begin
          Data := FInternalTCPClient.Socket.ReadByte;
          if FUpgraded and (Position = 0) and GetBit(Data, 7) then
          begin
            InFrame := True;
            Operation := ClearBit(Data, 7);
            Inc(Position);
          end
          else if FUpgraded and (Position = 1) then
          begin
            Masked := GetBit(Data, 7);
            Size := Data;
            if Masked then
              Size := Data - SetBit(0, 7);
            if Size = 0 then
              Position := 0
            else if Size = 126 then
              Size := FInternalTCPClient.Socket.ReadUInt16
            else if Size = 127 then
              Size := FInternalTCPClient.Socket.ReadUInt64;
            Inc(Position);
          end
          else if InFrame then
          begin
            ByteStream := ByteStream + [Data];
            if (FUpgraded and (Length(ByteStream) = Size)) then
            begin
              Position := 0;
              InFrame := False;
              if (Operation = $8) then
              begin
                FLock.Enter;
                try
                  FInternalTCPClient.Socket.Write(EncodeFrame(ByteStream, $9));
                finally
                  FLock.Leave;
                end;
              end
              else
              begin
                if FUpgraded and (not(Operation = $8)) then
                  FOnReceivedData(TArray<Byte>(ByteStream));
              end;
              SetLength(ByteStream, 0);
              if (Operation = $8) then
              begin
                if not FCloseHandshakeSent then
                  Self.Close;
                Break
              end;
            end;
          end;
        end;
      except
        on E: Exception do
          HandleException(E);
      end;
    end);
  if ((not Connected) or (not FUpgraded)) and
     (not((Operation = $8) or FCloseHandshakeSent)) then
    raise EWebSocketTCPClient.Create('Websocket not connected or timeout ' + QuotedStr(IndyTextEncoding_UTF8.GetString(ByteStream)));
end;

procedure TWebSocketTCPClient.Send(const Message: string);
begin
  Send(TArray<Byte>(IndyTextEncoding_UTF8.GetBytes(Message)));
end;

procedure TWebSocketTCPClient.Send(const Data: TArray<Byte>);
begin
  FLock.Enter;
  try
    FInternalTCPClient.Socket.Write(EncodeFrame(TIdBytes(Data), $1));
  finally
    FLock.Leave;
  end;
end;

procedure TWebSocketTCPClient.SendCloseHandshake;
begin
  FCloseHandshakeSent := True;
  FInternalTCPClient.Socket.Write(EncodeFrame([], $8));
  TThread.Sleep(200);
end;

function TWebSocketTCPClient.SetBit(
  const Value: Cardinal;
  const Byte: Byte): Cardinal;
begin
  Result := Value or (1 shl Byte);
end;

procedure TWebSocketTCPClient.SetOnError(const OnError: TWebSocketOnError);
begin
  FOnError := OnError;
end;

procedure TWebSocketTCPClient.SetOnReceiveData(const OnReceivedData: TWebSocketOnReceivedData);
begin
  FOnReceivedData := OnReceivedData;
end;

procedure TWebSocketTCPClient.CheckSsl(const URL: string);
var
  URI: IShared<TIdURI>;
begin
  URI := Shared.Make(TIdURI.Create(URL));

  if URI.Protocol.ToLower.Contains('wss') then
    URI.Protocol := ReplaceOnlyFirst(URI.Protocol.ToLower, 'wss', 'https')
  else
    URI.Protocol := ReplaceOnlyFirst(URI.Protocol.ToLower, 'ws', 'http');
  if URI.Protocol.ToLower.Equals('https') then
  begin
    if Assigned(FInternalTCPClient.IOHandler) and (FInternalTCPClient.IOHandler is TIdSSLIOHandlerSocketOpenSSL) then
      Exit;

    FInternalTCPClient.SetIOHandler(TIdSSLIOHandlerSocketOpenSSL.Create(FInternalTCPClient));
    TIdSSLIOHandlerSocketOpenSSL(FInternalTCPClient.IOHandler).SSLOptions.Mode := TIdSSLMode.sslmClient;
    TIdSSLIOHandlerSocketOpenSSL(FInternalTCPClient.IOHandler).SSLOptions.SSLVersions := [sslvSSLv2, sslvSSLv23, sslvSSLv3, sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
    TIdSSLIOHandlerSocketOpenSSL(FInternalTCPClient.IOHandler).SSLOptions.Method := FSSLVersion;
    TIdSSLIOHandlerSocketOpenSSL(FInternalTCPClient.IOHandler).PassThrough := False;
  end;
end;

function TWebSocketTCPClient.FormatURL(const URL: string): string;
var
  URI: IShared<TIdURI>;
  Port: Word;
begin
  URI := Shared.Make(TIdURI.Create(URL));

  if URI.Path.Trim.IsEmpty then
    URI.Path := '/';

  Port := StrToIntDef(URI.Port, 0);
  if (Port = 0) then
    if (URI.Protocol.ToLower.Equals('https') or URI.Protocol.ToLower.Equals('wss')) then
      Port := 443
    else
      Port := 80;

  URI.Port := IntToStr(Port);

  if not URI.Params.IsEmpty then
    Result := URI.Protocol + '://' + URI.Host + ':' + URI.Port + URI.Path + URI.Document + '?' + URI.Params
  else
    Result := URI.Protocol + '://' + URI.Host + ':' + URI.Port + URI.Path + URI.Document;

  if FURL.IsEmpty then
  begin
    FURL := Result;
    FInternalTCPClient.Host := URI.Host;
    FInternalTCPClient.Port := Port;
  end;

  CheckSsl(Result);
end;

{ TWebSocketIdTCPClient }

function TWebSocketIdTCPClient.Connected: Boolean;
begin
  try
    Result := inherited Connected;
  except
    Result := False;
  end;
end;

destructor TWebSocketIdTCPClient.Destroy;
begin
  if Assigned(FIOHandler) then
    FIOHandler.Free;

  inherited;
end;

end.
