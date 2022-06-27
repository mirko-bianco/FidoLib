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

unit Fido.Web.Client.WebSocket;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.SyncObjs,
  System.Math,
  System.DateUtils,

  IdURI,
  IdGlobal,
  IdTCPClient,
  IdSSLOpenSSL,
  IdCoderMIME,
  IdHashSHA,

  Spring,

  Fido.Exceptions,
  Fido.JSON.Marshalling,
  Fido.Web.Client.WebSocket.Intf;

type
  EWebSocketClient = class(EFidoException);

  TWebSocketClient = class(TInterfacedObject, IWebSocketClient)
  private type
    TInternalWebSocketClient = class(TIdTCPClient)
    strict private
      FURL: string;
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
    private
      procedure SendCloseHandshake;
      function ClearBit(const Value: Cardinal; const Byte: Byte): Cardinal;
      function GetBit(const Value: Cardinal; const Byte: Byte): Boolean;
      function SetBit(const Value: Cardinal; const Byte: Byte): Cardinal;
      function EncodeFrame(const Data: TIdBytes; const Operation: Byte): TIdBytes;
      procedure StartIncomingThread;
      function IsValidWebSocket: Boolean;
      function IsValidHeaders(const Headers: TStrings): Boolean;
      procedure Close;
      procedure HandleException(const Exception: Exception);
    public
      constructor Create(const URL: string; const WebsocketProtocol: string);
      destructor Destroy; override;

      procedure SetOnReceiveData(const OnReceivedData: TWebSocketOnReceivedData);
      procedure SetOnError(const OnError: TWebSocketOnError);

      procedure Connect; override;
      function Connected: Boolean; override;
      procedure Send(const Message: string); overload;
      procedure Send(const Data: TArray<Byte>); overload;
    end;

  private
    FEngine: TInternalWebSocketClient;
  protected
    function ConvertStringToDataEvent(const StringEvent: TWebSocketOnReceivedMessage): TWebSocketOnReceivedData;
  public
    constructor Create(const URL: string; const WebsocketProtocol: string = ''); reintroduce;
    destructor Destroy; override;

    procedure Send(const Message: string); overload;
    procedure Send(const Data: TArray<Byte>); overload;

    function Started: Boolean;
    procedure Start(const OnReceivedData: TWebSocketOnReceivedData; const OnError: TWebSocketOnError = nil); overload;
    procedure Start(const OnReceivedMessage: TWebSocketOnReceivedMessage; const OnError: TWebSocketOnError = nil); overload;
    procedure Stop;
  end;

  TWebSocketClient<T> = class(TWebSocketClient, IWebSocketClient<T>)
  private
    function ConvertItemToStringEvent(const ItemEvent: TWebSocketOnReceivedItem<T>): TWebSocketOnReceivedMessage;
  public
    procedure Send(const Item: T); overload;
    procedure Start(const OnReceivedItem: TWebSocketOnReceivedItem<T>; const OnError: TWebSocketOnError = nil); overload;
  end;

implementation

constructor TWebSocketClient.Create(
  const URL: string;
  const WebsocketProtocol: string);
begin
  inherited Create;

  FEngine := TInternalWebSocketClient.Create(URL, WebsocketProtocol);
end;

destructor TWebSocketClient.Destroy;
begin
  FEngine.Free;

  inherited;
end;

function TWebSocketClient.ConvertStringToDataEvent(const StringEvent: TWebSocketOnReceivedMessage): TWebSocketOnReceivedData;
begin
  Result := procedure(const Data: TArray<Byte>)
    begin
      StringEvent(IndyTextEncoding_UTF8.GetString(TIdBytes(Data)));
    end;
end;

procedure TWebSocketClient.Send(const Message: string);
begin
  FEngine.Send(Message);
end;

procedure TWebSocketClient.Send(const Data: TArray<Byte>);
begin
  FEngine.Send(Data);
end;

procedure TWebSocketClient.Start(
  const OnReceivedData: TWebSocketOnReceivedData;
  const OnError: TWebSocketOnError);
begin
  FEngine.SetOnReceiveData(OnReceivedData);
  FEngine.SetOnError(OnError);
  FEngine.Connect;
end;

procedure TWebSocketClient.Start(
  const OnReceivedMessage: TWebSocketOnReceivedMessage;
  const OnError: TWebSocketOnError);
begin
  FEngine.SetOnReceiveData(ConvertStringToDataEvent(OnReceivedMessage));
  FEngine.SetOnError(OnError);
  FEngine.Connect;
end;

function TWebSocketClient.Started: Boolean;
begin
  Result := FEngine.Connected;
end;

procedure TWebSocketClient.Stop;
begin
  FEngine.Close;
end;

{ TWebSocketClient.TInternalClientWebSocket }

function TWebSocketClient.TInternalWebSocketClient.ClearBit(
  const Value: Cardinal;
  const Byte: Byte): Cardinal;
begin
  Result := Value and not (1 shl Byte);
end;

procedure TWebSocketClient.TInternalWebSocketClient.Close;
begin
  if not Connected then
    Exit;
  FLock.Enter;
  try
    SendCloseHandshake;
    if Assigned(FIOHandler) then
    begin
      FIOHandler.InputBuffer.Clear;
      FIOHandler.CloseGracefully;
    end;
    Disconnect;
  finally
    FLock.Leave;
  end;
end;

procedure TWebSocketClient.TInternalWebSocketClient.Connect;
var
  URI: Shared<TIdURI>;
  IsSecure: Boolean;
begin
  if Connected then
    raise EWebSocketClient.Create('Connection already opened.');
  URI := TIdURI.Create(FURL);
  FCloseHandshakeSent := False;
  FHost := URI.Value.Host;
  if URI.Value.Protocol.ToLower.Contains('wss') then
    URI.Value.Protocol := ReplaceOnlyFirst(URI.Value.Protocol.ToLower, 'wss', 'https')
  else
    URI.Value.Protocol := ReplaceOnlyFirst(URI.Value.Protocol.ToLower, 'ws', 'http');
  if URI.Value.Path.Trim.IsEmpty then
    URI.Value.Path := '/';
  IsSecure := URI.Value.Protocol.ToLower.Equals('https');
  FPort := StrToIntDef(URI.Value.Port, 0);
  if (FPort = 0) then
    FPort := IfThen(IsSecure, 443, 80);
  if IsSecure then
  begin
    SetIOHandler(TIdSSLIOHandlerSocketOpenSSL.Create(Self));
    TIdSSLIOHandlerSocketOpenSSL(FIOHandler).SSLOptions.Mode := TIdSSLMode.sslmClient;
    TIdSSLIOHandlerSocketOpenSSL(FIOHandler).SSLOptions.SSLVersions := [TIdSSLVersion.sslvTLSv1, TIdSSLVersion.sslvTLSv1_1, TIdSSLVersion.sslvTLSv1_2];
    TIdSSLIOHandlerSocketOpenSSL(FIOHandler).SSLOptions.Method := sslvTLSv1_2;
    TIdSSLIOHandlerSocketOpenSSL(FIOHandler).PassThrough := False;
  end;
  inherited Connect;
  if not URI.Value.Port.IsEmpty then
    URI.Value.Host := URI.Value.Host + ':' + URI.Value.Port;
  if (URI.Value.Params <> '') then
    FSocket.WriteLn(Format('GET %s HTTP/1.1', [URI.Value.Path + URI.Value.Document + '?' + URI.Value.Params]))
  else
    FSocket.WriteLn(Format('GET %s HTTP/1.1', [URI.Value.Path + URI.Value.Document]));
  FSocket.WriteLn(Format('Host: %s', [URI.Value.Host]));
  FSocket.WriteLn('User-Agent: FidoLib websocket client');
  FSocket.WriteLn('Connection: keep-alive, Upgrade');
  FSocket.WriteLn('Upgrade: WebSocket');
  FSocket.WriteLn('Sec-WebSocket-Version: 13');
  FSocket.WriteLn(Format('Sec-WebSocket-Key: %s', [FKey]));
  if not FWebsocketProtocol.Trim.IsEmpty then
    FSocket.WriteLn(Format('Sec-WebSocket-Protocol: %s', [FWebsocketProtocol]));
  FSocket.WriteLn(EmptyStr);
  StartIncomingThread;
end;

function TWebSocketClient.TInternalWebSocketClient.Connected: Boolean;
begin
  try
    Result := inherited Connected;
  except
    Result := False;
  end;
end;

constructor TWebSocketClient.TInternalWebSocketClient.Create(
  const URL: string;
  const WebsocketProtocol: string);
var
  Bytes: TIdBytes;
  Index: Integer;
  Hash: Shared<TIdHashSHA1>;
begin
  inherited Create(nil);
  FClosing := False;
  FLock := TCriticalSection.Create;
  FURL := URL;
  FWebsocketProtocol := WebsocketProtocol;

  Randomize;

  SetLength(Bytes, 16);
  for Index := Low(Bytes) to High(Bytes) do
    Bytes[Index] := byte(random(255));
  FKey := TIdEncoderMIME.EncodeBytes(Bytes);

  Hash := TIdHashSHA1.Create;
  FExpectedAcceptResponse := TIdEncoderMIME.EncodeBytes(Hash.Value.HashString(FKey + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));
end;

destructor TWebSocketClient.TInternalWebSocketClient.Destroy;
begin
  FClosing := True;
  if FIncomingDataTask <> nil then
    FIncomingDataTask.Wait;
  FIncomingDataTask := nil;
  if Assigned(FIOHandler) then
    FIOHandler.Free;

  FLock.Free;
  inherited;
end;

function TWebSocketClient.TInternalWebSocketClient.EncodeFrame(
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

function TWebSocketClient.TInternalWebSocketClient.GetBit(
  const Value: Cardinal;
  const Byte: Byte): Boolean;
begin
  Result := (Value and (1 shl Byte)) <> 0;
end;

procedure TWebSocketClient.TInternalWebSocketClient.HandleException(const Exception: Exception);
var
  Disconnect: Boolean;
begin
  Disconnect := true;
  if Assigned(FOnError) then
    FOnError(Exception, Disconnect);
  if Disconnect then
    Self.Close;
end;

function TWebSocketClient.TInternalWebSocketClient.IsValidHeaders(const Headers: TStrings): Boolean;
begin
  Result := False;
  Headers.NameValueSeparator := ':';
  if (Headers.Count = 0) then
    Exit;
  if (not Headers[0].Contains('HTTP/1.1 101')) and
     (Headers[0].Contains('HTTP/1.1')) then
    raise EWebSocketClient.Create(Headers[0].Substring(9));
  if Headers.Values['Connection'].Trim.ToLower.Equals('upgrade') and
     Headers.Values['Upgrade'].Trim.ToLower.Equals('websocket') then
  begin
    if Headers.Values['Sec-WebSocket-Accept'].Trim.Equals(FExpectedAcceptResponse) then
      Exit(True);
    if Headers.Values['Sec-WebSocket-Accept'].Trim.IsEmpty then
      Exit(True);
    raise EWebSocketClient.Create('Could not perform handshake');
  end;
end;

function TWebSocketClient.TInternalWebSocketClient.IsValidWebSocket: Boolean;
var
  StringStream: string;
  Data: Byte;
  Headers: Shared<TStringlist>;
begin
  Result := False;
  StringStream := EmptyStr;
  Headers := TStringList.Create;
  try
    FUpgraded := False;
    while Connected and not FUpgraded do
    begin
      Data := FSocket.ReadByte;
      StringStream := StringStream + Chr(Data);
      if (not FUpgraded) and (Data = Ord(#13)) then
      begin
        if (StringStream = #10#13) then
        begin
          if not IsValidHeaders(Headers) then
            raise EWebSocketClient.Create('Invalid header');
          FUpgraded := True;
          StringStream := EmptyStr;
        end
        else
        begin
          Headers.Value.Add(StringStream.Trim);
          StringStream := EmptyStr;
        end;
      end;
    end;
    Result := True;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TWebSocketClient.TInternalWebSocketClient.StartIncomingThread;
var
  Operation: Byte;
  ByteStream: TIdBytes;
begin
  if not IsValidWebSocket then
    Exit;
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
          Data := FSocket.ReadByte;
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
              Size := FSocket.ReadUInt16
            else if Size = 127 then
              Size := FSocket.ReadUInt64;
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
                  FSocket.Write(EncodeFrame(ByteStream, $9));
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
    raise EWebSocketClient.Create('Websocket not connected or timeout ' + QuotedStr(IndyTextEncoding_UTF8.GetString(ByteStream)));
end;

procedure TWebSocketClient.TInternalWebSocketClient.Send(const Message: string);
begin
  Send(TArray<Byte>(IndyTextEncoding_UTF8.GetBytes(Message)));
end;

procedure TWebSocketClient.TInternalWebSocketClient.Send(const Data: TArray<Byte>);
begin
  FLock.Enter;
  try
    FSocket.Write(EncodeFrame(TIdBytes(Data), $1));
  finally
    FLock.Leave;
  end;
end;

procedure TWebSocketClient.TInternalWebSocketClient.SendCloseHandshake;
begin
  FCloseHandshakeSent := True;
  FSocket.Write(EncodeFrame([], $8));
  TThread.Sleep(200);
end;

function TWebSocketClient.TInternalWebSocketClient.SetBit(
  const Value: Cardinal;
  const Byte: Byte): Cardinal;
begin
  Result := Value or (1 shl Byte);
end;

procedure TWebSocketClient.TInternalWebSocketClient.SetOnError(const OnError: TWebSocketOnError);
begin
  FOnError := OnError;
end;

procedure TWebSocketClient.TInternalWebSocketClient.SetOnReceiveData(const OnReceivedData: TWebSocketOnReceivedData);
begin
  FOnReceivedData := OnReceivedData;
end;

{ TWebSocketClient<T> }

function TWebSocketClient<T>.ConvertItemToStringEvent(const ItemEvent: TWebSocketOnReceivedItem<T>): TWebSocketOnReceivedMessage;
begin
  Result := procedure(const Message: string)
    begin
      ItemEvent(JSONUnmarshaller.To<T>(Message));
    end;
end;

procedure TWebSocketClient<T>.Send(const Item: T);
begin
  Send(JSONMarshaller.From<T>(Item));
end;

procedure TWebSocketClient<T>.Start(const OnReceivedItem: TWebSocketOnReceivedItem<T>; const OnError: TWebSocketOnError);
begin
  FEngine.SetOnReceiveData(ConvertStringToDataEvent(ConvertItemToStringEvent(OnReceivedItem)));
  FEngine.SetOnError(OnError);
  FEngine.Connect;
end;

end.
