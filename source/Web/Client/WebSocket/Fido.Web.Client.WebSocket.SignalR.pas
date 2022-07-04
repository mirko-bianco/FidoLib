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

unit Fido.Web.Client.WebSocket.SignalR;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  System.JSON,
  System.Generics.Collections,

  IdURI,
  IdHTTP,
  IdSSLOpenSSL,

  Spring,
  Spring.Collections,

  Fido.Boxes,
  Fido.Exceptions,
  Fido.Utilities,
  Fido.JSON.Marshalling,
  Fido.Web.Client.WebSocket.Intf,
  Fido.Web.Client.WebSocket.SignalR.Types;

type
  ESignalR = class(EFidoException);

  TTSignalROnReceivedMessage = reference to procedure(const Channel: string; const Message: string);
  TTryProcessJson = reference to function(const JsonObject: TJSONObject): Boolean;
  TOnMethodResultReceived = reference to function(const Message: string): TSignalRMethodResult;

  IStringReadonlyList = IReadonlyList<string>;

  TSignalR = record
  private var
    FWebSocketClient: IWebSocketClient;
    FCallId: IBox<Int64>;

    FHost: string;
    FSecured: Boolean;
    FCustomHeaders: Shared<TStrings>;
    FOnError: TWebSocketOnError;
    FOnReceivedWebsocketMessage: TWebSocketOnReceivedMessage;

    FLock: IReadWriteSync;
    FResultEventsMethodsMap: IDictionary<Int64, TOnMethodResultReceived>;
    FMethodsResultsMap: IDictionary<Int64, TSignalRMethodResult>;
  private
    function NextCallId: Int64;
    function Negotiate(const Host: string; const Secured: Boolean; const CustomHeaders: TStrings): INegotiationResponse;
    procedure Connect(const Host: string; const Secured: Boolean; const NegotiationResponse: INegotiationResponse; const OnReceivedMessage: TWebSocketOnReceivedMessage; const OnError: TWebSocketOnError);
  public
    class operator Initialize(out Dest: TSignalR);

    constructor Create(const Host: string; const Secured: Boolean; const OnReceivedMessage: TTSignalROnReceivedMessage; const WebSocketClient: IWebSocketClient; const OnError: TWebSocketOnError = nil); overload;
    constructor Create(const Host: string; const Secured: Boolean; const CustomHeaders: TStrings; const OnReceivedMessage: TTSignalROnReceivedMessage; const WebSocketClient: IWebSocketClient; const OnError: TWebSocketOnError = nil); overload;

    function Send<TArguments>(const Hub: string; const Method: string; const Arguments: TArguments; const OnMethodResultReceived: TOnMethodResultReceived): TSignalRMethodResult; overload;
    function Send(const Hub: string; const Method: string; const Arguments: string; const OnMethodResultReceived: TOnMethodResultReceived): TSignalRMethodResult; overload;

    procedure Start;
    procedure Stop;
    function Started: Boolean;
  end;

implementation

{ TSignalR }

constructor TSignalR.Create(
  const Host: string;
  const Secured: Boolean;
  const CustomHeaders: TStrings;
  const OnReceivedMessage: TTSignalROnReceivedMessage;
  const WebSocketClient: IWebSocketClient;
  const OnError: TWebSocketOnError);
var
  LLock: IReadWriteSync;
  LResultEventsMap: IDictionary<Int64, TOnMethodResultReceived>;
  LMethodsResultsMap: IDictionary<Int64, TSignalRMethodResult>;
begin
  Guard.CheckNotNull<TTSignalROnReceivedMessage>(OnReceivedMessage, 'OnReceivedMessage');

  FHost := Host;
  FSecured := Secured;
  FCustomHeaders := TStringList.Create;
  FCustomHeaders.Value.AddStrings(CustomHeaders);
  FWebSocketClient := Utilities.CheckNotNullAndSet(WebSocketClient, 'WebSocketClient');
  FOnError := OnError;

  LLock := FLock;
  LResultEventsMap := FResultEventsMethodsMap;
  LMethodsResultsMap := FMethodsResultsMap;

 {$REGION ' OnReceivedWebsocketMessage '}
  FOnReceivedWebsocketMessage := procedure(const Message: string)
    var
      JsonObject: Shared<TJSONObject>;
      TryProcessMethodsResults: TTryProcessJson;
      TryReceivedData: TTryProcessJson;
    begin
      if Message = '{}' then
        Exit;

      JsonObject := TJSONValue.ParseJSONValue(Message) as TJSONObject;
      if not Assigned(JsonObject.Value) then
        Exit;

     {$REGION ' Manage methods callbacks '}
      TryProcessMethodsResults := function(const JsonObject: TJSONObject): Boolean
        var
          Value: TJSONValue;
          Identifier: TJSONString;
          OnMethodResultReceived: TOnMethodResultReceived;
        begin
          Result := True;
          Value := JsonObject.FindValue('I');
          if not Assigned(Value) then
            Exit(False);

          Identifier := Value as TJSONString;

          LLock.BeginRead;
          try
            LResultEventsMap.TryGetValue(StrToInt(Identifier.Value), OnMethodResultReceived);
          finally
            LLock.EndRead;
          end;
          if Assigned(OnMethodResultReceived) then
          begin
            LLock.BeginWrite;
            try
              LMethodsResultsMap[StrToInt(Identifier.Value)] := OnMethodResultReceived(JsonObject.ToJSON);
              LResultEventsMap.Remove(StrToInt(Identifier.Value));
            finally
              LLock.EndWrite;
            end;
          end;
        end;

      if TryProcessMethodsResults(JsonObject) then
        Exit;
     {$ENDREGION}
     {$REGION ' Manage received Data '}
      TryReceivedData := function(const JsonObject: TJSONObject): Boolean
        var
          Value: TJSONValue;
          DataContainerArray: TJSONArray;
          Index: Integer;
          Method: TJSONString;
          DataArray: TJSONArray;
        begin
          Result := True;
          Value := JsonObject.FindValue('C');
          if not Assigned(Value) then
            Exit(False);

          Value := JsonObject.FindValue('M');
          if not Assigned(Value) then
            Exit(False);
          DataContainerArray := Value as TJSONArray;

          for Index := 0 to DataContainerArray.Count - 1 do
          begin
            Method := DataContainerArray[Index].P['M'] as TJSONString;
            DataArray := DataContainerArray[Index].P['A'] as TJSONArray;

            OnReceivedMessage(Method.Value, DataArray.ToString);
          end;
        end;

      if TryReceivedData(JsonObject) then
        Exit;
     {$ENDREGION}
    end;
 {$ENDREGION}
end;

constructor TSignalR.Create(
  const Host: string;
  const Secured: Boolean;
  const OnReceivedMessage: TTSignalROnReceivedMessage;
  const WebSocketClient: IWebSocketClient;
  const OnError: TWebSocketOnError);
var
  CustomHeaders: Shared<TStrings>;
begin
  CustomHeaders := TStringList.Create;
  Create(Host, Secured, CustomHeaders, OnReceivedMessage, WebSocketClient, OnError);
end;

class operator TSignalR.Initialize(out Dest: TSignalR);
begin
  Dest.FCallId := Box<Int64>.Setup(0);
  Dest.FLock := TMREWSync.Create;

  Dest.FResultEventsMethodsMap := TCollections.createDictionary<Int64, TOnMethodResultReceived>;
  Dest.FMethodsResultsMap := TCollections.createDictionary<Int64, TSignalRMethodResult>;
end;

procedure TSignalR.Start;
var
  Response: INegotiationResponse;
  Success: Boolean;
begin
  repeat
    Response := Negotiate(FHost, FSecured, FCustomHeaders);
    try
      Connect(FHost, FSecured, Response, FOnReceivedWebsocketMessage, FOnError);
      Success := True;
    except
      Success := False;
    end;
  until Success;
end;

function TSignalR.Started: Boolean;
begin
  Result := FWebSocketClient.Started;
end;

function TSignalR.NextCallId: Int64;
begin
  FCallId.UpdateValue(FCallId.Value + 1);
  Result := FCallId.Value;
end;

function TSignalR.Negotiate(
  const Host: string;
  const Secured: Boolean;
  const CustomHeaders: TStrings): INegotiationResponse;
var
  Response: string;
  Url: string;
begin
  Url := TIdURI.URLEncode(
    Utilities.IfThen<string>(Secured, 'https://', 'http://') + Host + '/SignalR/negotiate');

  Response := FWebSocketClient.Get(Url, CustomHeaders);
  Result := JSONUnmarshaller.To<INegotiationResponse>(Response);
  if not Result.TryWebSockets then
    raise ESignalR.CreateFmt('SignalR Host %s does not support websockets', [Host]);
end;

procedure TSignalR.Connect(
  const Host: string;
  const Secured: Boolean;
  const NegotiationResponse: INegotiationResponse;
  const OnReceivedMessage: TWebSocketOnReceivedMessage;
  const OnError: TWebSocketOnError);
var
  Url: string;
  CustomHeaders: Shared<TStringList>;
begin
  Url := TIdURI.URLEncode(Utilities.IfThen(Secured, 'https://', 'http://') + Host + '/SignalR/connect' +
    '?transport=webSockets' +
    '&clientProtocol=' + NegotiationResponse.ProtocolVersion +
    '&connectionToken=' + NegotiationResponse.ConnectionToken +
    '&connectionId=' + NegotiationResponse.ConnectionId);

  CustomHeaders := TStringList.Create;

  FWebSocketClient.Start(
    Url,
    CustomHeaders.Value,
    OnReceivedMessage,
    OnError);
end;

procedure TSignalR.Stop;
begin
  FWebSocketClient.Stop;
end;

function TSignalR.Send<TArguments>(
  const Hub: string;
  const Method: string;
  const Arguments: TArguments;
  const OnMethodResultReceived: TOnMethodResultReceived): TSignalRMethodResult;
begin
  Result := Send(Hub, Method, JSONMarshaller.From<TArguments>(Arguments), OnMethodResultReceived);
end;

function TSignalR.Send(
  const Hub: string;
  const Method: string;
  const Arguments: string;
  const OnMethodResultReceived: TOnMethodResultReceived): TSignalRMethodResult;
var
  Identifier: Int64;
  Executed: Boolean;
begin
  if not FWebSocketClient.Started then
    Exit(TSignalRMethodResult.Create(False, 'SignalR websocket is not started.'));

  Identifier := NextCallId;

  FWebSocketClient.Send(Format('{"H": "%s", "M": "%s", "A": [%s], "I": %d}',
    [Hub, Method, Arguments, Identifier]));

  FLock.BeginWrite;
  try
    FResultEventsMethodsMap[Identifier] := OnMethodResultReceived;
  finally
    FLock.EndWrite;
  end;

  repeat
    Sleep(10);

    FLock.BeginRead;
    try
      Executed := FMethodsResultsMap.ContainsKey(Identifier);
    finally
      FLock.EndRead;
    end;

    if not Executed then
      Continue;

    Result := FMethodsResultsMap[Identifier];
    FLock.BeginWrite;
    try
      FMethodsResultsMap.Remove(Identifier);
    finally
      FLock.EndWrite;
    end;
  until Executed;
end;

end.
