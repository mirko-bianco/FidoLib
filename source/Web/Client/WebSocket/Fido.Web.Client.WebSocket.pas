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

  IdGlobal,

  Fido.Exceptions,
  Fido.Utilities,
  Fido.JSON.Marshalling,
  Fido.Web.Client.WebSocket.Intf,
  Fido.Web.Client.WebSocket.TcpClient.Intf;

type
  TWebSocketClient = class(TInterfacedObject, IWebSocketClient)
  private
    FEngine: IWebSocketTCPClient;
  protected
    function ConvertStringToDataEvent(const StringEvent: TWebSocketOnReceivedMessage): TWebSocketOnReceivedData;
  public
    constructor Create(const Engine: IWebSocketTCPClient);

    function Get(const URL: string; const CustomHeaders: TStrings): string;

    procedure Send(const Message: string); overload;
    procedure Send(const Data: TArray<Byte>); overload;

    function Started: Boolean;
    procedure Start(const URL: string; const CustomHeaders: TStrings; const OnReceivedData: TWebSocketOnReceivedData; const OnError: TWebSocketOnError = nil; const WebsocketProtocol: string = ''); overload;
    procedure Start(const URL: string; const CustomHeaders: TStrings; const OnReceivedMessage: TWebSocketOnReceivedMessage; const OnError: TWebSocketOnError = nil; const WebsocketProtocol: string = ''); overload;
    procedure Stop;
  end;

  TWebSocketClient<T> = class(TWebSocketClient, IWebSocketClient<T>)
  private
    function ConvertItemToStringEvent(const ItemEvent: TWebSocketOnReceivedItem<T>): TWebSocketOnReceivedMessage;
  public
    procedure Send(const Item: T); overload;
    procedure Start(const URL: string; const CustomHeaders: TStrings; const OnReceivedItem: TWebSocketOnReceivedItem<T>; const OnError: TWebSocketOnError = nil; const WebsocketProtocol: string = ''); overload;
  end;

implementation

constructor TWebSocketClient.Create(const Engine: IWebSocketTCPClient);
begin
  inherited Create;

  FEngine := Utilities.CheckNotNullAndSet(Engine, 'Engine');
end;

function TWebSocketClient.Get(
  const URL: string;
  const CustomHeaders: TStrings): string;
begin
  Result := FEngine.Get(URL, CustomHeaders);
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
  const URL: string;
  const CustomHeaders: TStrings;
  const OnReceivedData: TWebSocketOnReceivedData;
  const OnError: TWebSocketOnError;
  const WebsocketProtocol: string);
begin
  FEngine.SetOnReceiveData(OnReceivedData);
  FEngine.SetOnError(OnError);
  FEngine.Connect(URL, CustomHeaders, WebsocketProtocol);
end;

procedure TWebSocketClient.Start(
  const URL: string;
  const CustomHeaders: TStrings;
  const OnReceivedMessage: TWebSocketOnReceivedMessage;
  const OnError: TWebSocketOnError;
  const WebsocketProtocol: string);
begin
  FEngine.SetOnReceiveData(ConvertStringToDataEvent(OnReceivedMessage));
  FEngine.SetOnError(OnError);
  FEngine.Connect(URL, CustomHeaders, WebsocketProtocol);
end;

function TWebSocketClient.Started: Boolean;
begin
  Result := FEngine.Connected;
end;

procedure TWebSocketClient.Stop;
begin
  FEngine.Close;
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

procedure TWebSocketClient<T>.Start(
  const URL: string;
  const CustomHeaders: TStrings;
  const OnReceivedItem: TWebSocketOnReceivedItem<T>;
  const OnError: TWebSocketOnError;
  const WebsocketProtocol: string);
begin
  FEngine.SetOnReceiveData(ConvertStringToDataEvent(ConvertItemToStringEvent(OnReceivedItem)));
  FEngine.SetOnError(OnError);
  FEngine.Connect(URL, CustomHeaders, WebsocketProtocol);
end;

end.
