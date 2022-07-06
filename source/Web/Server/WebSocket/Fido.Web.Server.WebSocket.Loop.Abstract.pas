(*
 * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
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

unit Fido.Web.Server.WebSocket.Loop.Abstract;

interface

uses
  System.Classes,
  System.SysUtils,

  Fido.Http.ResponseInfo.Intf,
  Fido.Http.Response.Intf,
  Fido.Http.Types,

  Fido.Web.Server.WebSocket.Loop.intf;

type
  TLoopServerWebSocket = class abstract (TInterfacedObject, ILoopServerWebSocket)
  private type
    TMessageRecord = record
    private
      FData : TWSBytes;
      FStart: Integer;
      FSize : Integer;

      procedure GetSize(Len: Integer);
      function DataSize: Int64;
      function Ping: Boolean;
      function Pong: TWSBytes;
      function GetData(var AData: TBytes; var IsText: Boolean): Boolean;
      procedure Consume;
      function Close: Boolean;
      procedure CloseMessage;
    public
      procedure SetText(const Str: string);
      procedure SetData(const Data: TBytes; const IsText: Boolean = False);
    end;

  private var
    FHttpResponse: IHttpResponse;
    FThread: TThread;
    FLoopThread: TThread;

    procedure ReadLoop;
    function IsAlive: Boolean;
  protected
    procedure OnReceivedData(const Data: TBytes); virtual; abstract;
    procedure OnReceivedMessage(const Str: string); virtual; abstract;
    procedure Step; virtual; abstract;
    function GetLoopDuration: Integer; virtual; abstract;
  public
    constructor Create(const Key: string; const HttpResponse: IHttpResponse);
    destructor Destroy; override;

    procedure SendMessage(const Str: string);
    procedure SendData(const Data: TBytes; var IsText: Boolean);
    procedure Close;

    procedure Run;
  end;

  TServerWebSocketClass = class of TLoopServerWebSocket;

implementation

procedure TLoopServerWebSocket.TMessageRecord.SetText(const Str: string);
begin
  SetData(TEncoding.UTF8.GetBytes(Str), True);
end;

procedure TLoopServerWebSocket.TMessageRecord.SetData(
  const Data: TBytes;
  const IsText: Boolean = False);
var
  DataLength: Int64;
  X, C: Integer;
  Index: Integer;
begin
  DataLength := Length(Data);
  C := 2 + DataLength;
  if DataLength > 125 then
  begin
    Inc(C, 2);
    if DataLength > 65535 then
      Inc(C, 6);
  end;
  SetLength(FData, C);
  if IsText then
    FData[0] := $81  // FIN, TEXT
  else
    FData[0] := $82; // FIN, Binary
  X := 1;
  if DataLength < 126 then
  begin
    FData[X] := DataLength; // MASK=0|LEN=??????
    Inc(X);
  end else begin
    if DataLength < 65565 then
    begin
      FData[X] := 126;
      C := 2;
    end else begin
      FData[X] := 127;
      C := 8;
    end;
    Inc(X, C);
    for Index := 0 to C - 1 do
    begin
      FData[X] := DataLength and $FF;
      Dec(X);
      DataLength := DataLength shr 8;
    end;
    Inc(X, C + 1);
    DataLength := Length(Data);
  end;
  Move(Data[0], FData[X], DataLength);
end;

procedure TLoopServerWebSocket.TMessageRecord.CloseMessage;
begin
  SetLength(FData, 2);
  FData[0] := $88; // FIN + CLOSE
  FData[1] := $00; // Empty message
end;

procedure TLoopServerWebSocket.TMessageRecord.GetSize(Len: Integer);
var
  Index: Integer;
begin
  FSize := 0;
  for Index := 0 to Len - 1 do
  begin
    Assert(Length(FData) >= FStart);
    FSize := FSize shl 8 + FData[FStart];
    Inc(FStart);
  end;
end;

function TLoopServerWebSocket.TMessageRecord.DataSize: Int64;
begin
  Assert(Length(FData) >= 2);
  FStart := 2; // Head
  FSize := FData[1] and $7F;
  case FSize of
    126:
    begin
      Assert(Length(FData) >= 4);
      GetSize(2);
    end;
    127:
    begin
      Assert(Length(FData) >= 10);
      GetSize(8);
    end;
  end;
  Inc(FStart, 4); // Mask
  Result := FSize;
end;

function TLoopServerWebSocket.TMessageRecord.Ping: Boolean;
var
  Index: Integer;
begin
  Assert((FStart > 0) and (Length(FData) >= FStart + FSize));
  if (FData[1] and $80) = 0 then // Not masked
  begin
    FData[0] := $88; // FIN + CLOSE
    Result := False;
  end else begin
    // Unmask
    for Index := 0 to FSize - 1 do
    begin
      FData[FStart + Index] := FData[FStart + Index] xor FData[FStart - 4 + Index mod 4];
    end;
    Result := FData[0] = $89; // FIN + PING
  end;
end;

function TLoopServerWebSocket.TMessageRecord.Pong: TWSBytes;
begin
  Assert((FStart > 0) and (Length(FData) >= FStart + FSize));
  SetLength(Result, FStart - 4 + FSize);
  Result[0] := $8A; // Pong
  Move(FData[1], Result[1], FStart - 5); // Size
  Result[1] := Result[1] and $7F;  // not masked
  Move(FData[FStart], Result[FStart - 4], FSize); // Payload
  Consume;
end;

function TLoopServerWebSocket.TMessageRecord.GetData(
  var AData: TBytes;
  var IsText: Boolean): Boolean;
var
  Index: Integer;
begin
  Assert((FStart > 0) and (Length(FData) >= FStart + FSize));
  if FData[0] = $88 then // FIN + CLOSE
    Result := False
  else begin
    Index := Length(AData);
    SetLength(AData, Index + FSize);
    Move(FData[FStart], AData[Index], FSize);
    Result := FData[0] and $80 > 0; // FIN
    IsText := FData[0] and $7F = 1; // Text
    Consume;
  end;
end;

function TLoopServerWebSocket.TMessageRecord.Close: Boolean;
begin
  Result := (Length(FData) > 0) and (FData[0] = $88); // FIN + CLOSE
end;

procedure TLoopServerWebSocket.TMessageRecord.Consume;
begin
  Assert((FStart > 0) and (Length(FData) >= FStart + FSize));
  Delete(FData, 0, FStart + FSize);
end;

procedure TLoopServerWebSocket.Close;
var
  MessageRecord: TMessageRecord;
begin
  MessageRecord.CloseMessage;
  FHttpResponse.WriteBytesToWebSocket(MessageRecord.FData);
  FHttpResponse.DisconnectWebSocket;
end;

constructor TLoopServerWebSocket.Create(
  const Key: string;
  const HttpResponse: IHttpResponse);
begin
  inherited Create;
  FHttpResponse := HttpResponse;

  FHttpResponse.HeaderParams.Clear;
  FHttpResponse.HeaderParams['Upgrade'] := 'websocket';
  FHttpResponse.HeaderParams['Connection'] :=  'Upgrade';
  FHttpResponse.HeaderParams['Sec-WebSocket-Accept'] := FHttpResponse.GetWebSocketSignature(Key);

  FHttpResponse.SetResponseCode(101, 'Switching Protocols');
  FHttpResponse.WriteHeader;

  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      while not FThread.CheckTerminated do
      begin
        Step;
        Sleep(GetLoopDuration);
      end
    end);
  FThread.Start;

  FLoopThread := TThread.CreateAnonymousThread(ReadLoop);
  FLoopThread.Start;

  Sleep(10);
end;

destructor TLoopServerWebSocket.Destroy;
begin
  if not FThread.CheckTerminated then
    FThread.Terminate;
  if not FLoopThread.CheckTerminated then
    FLoopThread.Terminate;
  inherited;
end;

function TLoopServerWebSocket.IsAlive: Boolean;
begin
  Result := not FLoopThread.CheckTerminated;
end;

procedure TLoopServerWebSocket.SendData(
  const Data: TBytes;
  var IsText: Boolean);
var
  WebSocketMessage: TMessageRecord;
begin
  WebSocketMessage.SetData(Data, IsText);
  FHttpResponse.WriteBytesToWebSocket(WebSocketMessage.FData);
end;

procedure TLoopServerWebSocket.SendMessage(const Str: string);
var
  WebSocketMessage: TMessageRecord;
begin
  WebSocketMessage.SetText(Str);
  FHttpResponse.WriteBytesToWebSocket(WebSocketMessage.FData);
end;

procedure TLoopServerWebSocket.ReadLoop;
var
  M: TMessageRecord;
  D: TBytes;
  T: Boolean;
begin
  repeat
    M.FData := nil;
    FHttpResponse.ReadBytesFromWebSocket(M.FData, 6, True); // 2 head + 4 mask
    if (M.FData = nil) or (M.FData[0] = $88) then
      Break;
    case M.FData[1] and $7F of
      126: FHttpResponse.ReadBytesFromWebSocket(M.FData, 2, True); // word  Size
      127: FHttpResponse.ReadBytesFromWebSocket(M.FData, 8, True); // Int64 Size
    end;
    FHttpResponse.ReadBytesFromWebSocket(M.FData, M.DataSize, True);
    if M.Ping then
      FHttpResponse.WriteBytesToWebSocket(M.Pong)
    else
    begin
      if M.GetData(D, T) then
      begin
        if T then
          OnReceivedMessage(TEncoding.UTF8.GetString(D))
        else
          OnReceivedData(D);
        D := nil;
      end;
    end;
  until M.Close; // FIN + CLOSE
  if not FThread.CheckTerminated then
    FThread.Terminate;
  FHttpResponse.DisconnectWebSocket;
end;

procedure TLoopServerWebSocket.Run;
begin
  repeat
    Sleep(100);
  until not Self.IsAlive;
  Close;
end;

end.
