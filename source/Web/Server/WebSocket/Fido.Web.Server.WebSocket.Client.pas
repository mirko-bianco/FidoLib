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

unit Fido.Web.Server.WebSocket.Client;

interface

uses
  System.StrUtils,
  System.SysUtils,
  System.Classes,
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

  Fido.Http.Utils,
  Fido.JSON.Marshalling;

type
  TWebSocketClient = class
  strict private
    FContext: TIdContext;
    FTopic: string;
  public
    constructor Create(const Context: TIdContext; const Topic: string);

    function WaitForData: TArray<Byte>;
    function WaitForMessage: string;
    function WaitFor<T>: T;
    procedure Send(const Data: TArray<Byte>); overload;
    procedure Send(const Message: string); overload;
    procedure Send<T>(const Item: T); overload;

    property Topic: string read FTopic;
    function Host: string;
  end;

implementation

constructor TWebSocketClient.Create(
  const Context: TIdContext;
  const Topic: string);
begin
  inherited Create;

  FContext := Context;
  FTopic := Topic;
end;

function TWebSocketClient.WaitForData: TArray<Byte>;
var
  Data: TArray<Byte>;
  AByte: Byte;
  Bytes: array[0..7] of Byte;
  Index: int64;
  DecodedSize: int64;
  Mask: array[0..3] of byte;
begin
  FContext.Connection.IOHandler.CheckForDataOnSource(10);

  try
    if FContext.Connection.IOHandler.ReadByte = $81 then
    begin
      AByte := FContext.Connection.IOHandler.ReadByte;
      case AByte of
        $FE: begin
          Bytes[1] := FContext.Connection.IOHandler.ReadByte;
          Bytes[0] := FContext.Connection.IOHandler.ReadByte;
          Bytes[2] := 0;
          Bytes[3] := 0;
          Bytes[4] := 0;
          Bytes[5] := 0;
          Bytes[6] := 0;
          Bytes[7] := 0;
          DecodedSize := Int64(Bytes);
        end;
        $FF: begin
          Bytes[7] := FContext.Connection.IOHandler.ReadByte;
          Bytes[6] := FContext.Connection.IOHandler.ReadByte;
          Bytes[5] := FContext.Connection.IOHandler.ReadByte;
          Bytes[4] := FContext.Connection.IOHandler.ReadByte;
          Bytes[3] := FContext.Connection.IOHandler.ReadByte;
          Bytes[2] := FContext.Connection.IOHandler.ReadByte;
          Bytes[1] := FContext.Connection.IOHandler.ReadByte;
          Bytes[0] := FContext.Connection.IOHandler.ReadByte;
          DecodedSize := Int64(Bytes);
        end;
        else
          DecodedSize := AByte - 128;
      end;
      Mask[0] := FContext.Connection.IOHandler.ReadByte;
      Mask[1] := FContext.Connection.IOHandler.ReadByte;
      Mask[2] := FContext.Connection.IOHandler.ReadByte;
      Mask[3] := FContext.Connection.IOHandler.ReadByte;
      if DecodedSize < 1 then
      begin
        Result := [];
        Exit;
      end;
      SetLength(Data, DecodedSize);
      FContext.Connection.IOHandler.ReadBytes(TIdBytes(Data), DecodedSize, False);
      for Index := 0 to DecodedSize - 1 do
        Data[Index] := Data[Index] xor Mask[Index mod 4];
    end;
  except
  end;

  Result := Data;
end;

function TWebSocketClient.WaitForMessage: string;
begin
  Result := IndyTextEncoding_UTF8.GetString(TIdBytes(WaitForData));
end;

function TWebSocketClient.WaitFor<T>: T;
begin
  Result := JSONUnmarshaller.To<T>(WaitForMessage);
end;

function TWebSocketClient.Host: string;
begin
  Result := FContext.Binding.PeerIP;
end;

procedure TWebSocketClient.Send(const Data: TArray<Byte>);
var
  DataLen: Integer;
  Bytes: TArray<Byte>;
begin
  DataLen := Length(Data);
  Bytes := [$81];
  if DataLen <= 125 then
    Bytes := Bytes + [DataLen]
  else if (DataLen >= 126) and (DataLen <= 65535) then
    Bytes := Bytes + [
      126,
      (DataLen shr 8) and 255,
      DataLen and 255]
  else
    Bytes := Bytes + [
      127,
      (int64(DataLen) shr 56) and 255,
      (int64(DataLen) shr 48) and 255,
      (int64(DataLen) shr 40) and 255,
      (int64(DataLen) shr 32) and 255,
      (DataLen shr 24) and 255,
      (DataLen shr 16) and 255,
      (DataLen shr 8) and 255,
      DataLen and 255];
  Bytes := Bytes + Data;
  try
    FContext.Connection.IOHandler.Write(TIdBytes(Bytes), Length(Bytes));
  except
  end;
end;

procedure TWebSocketClient.Send(const Message: string);
begin
  Send(TArray<Byte>(IndyTextEncoding_UTF8.GetBytes(Message)));
end;

procedure TWebSocketClient.Send<T>(const Item: T);
begin
  Send(JSONMarshaller.From<T>(Item));
end;

end.
