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

unit Fido.Web.Server.WebSocket.Tool;

interface

uses
  System.RegularExpressions,
  System.SysUtils,
  System.Rtti,

  Spring.Collections,

  Fido.Http.Request.Intf,
  Fido.Http.Response.Intf,

  Fido.Web.Server.WebSocket.Loop.Abstract,
  Fido.Web.Server.WebSocket.Loop.Intf;

type
  ServerWebSocket = class
    class function DetectLoop(const WebSockets: IDictionary<string, TClass>; const HttpRequest: IHttpRequest; const HttpResponse: IHttpResponse; out WebSocket: ILoopServerWebSocket): Boolean;
  end;

implementation

class function ServerWebSocket.DetectLoop(
  const WebSockets: IDictionary<string, TClass>;
  const HttpRequest: IHttpRequest;
  const HttpResponse: IHttpResponse;
  out WebSocket: ILoopServerWebSocket): Boolean;
var
  Key, Ver: string;
  WebSocketClass: TClass;
  Context: TRttiContext;
  InstanceType: TRttiInstanceType;
begin
  WebSocketClass := nil;
  with WebSockets.Keys.GetEnumerator do
    while MoveNext do
      if TRegEx.IsMatch(HttpRequest.URI.ToUpper, Current.ToUpper) then
        WebSocketClass := WebSockets.Items[Current];
  if WebSocketClass = nil then
    if not WebSockets.TryGetValue('$', WebSocketClass) then
      Exit(False);

  Key := HttpRequest.HeaderParams.GetValueOrDefault('Sec-WebSocket-Key');
  Ver := HttpRequest.HeaderParams.GetValueOrDefault('Sec-WebSocket-Version');
  if (Key <> '') and (Ver = '13') then
  begin
    Context := TRttiContext.Create;
    InstanceType := Context.GetType(WebSocketClass).AsInstance;
    WebSocket := InstanceType.GetMethod('Create').Invoke(
      WebSocketClass,
      [TValue.From<string>(Key), TValue.From<IHttpResponse>(HttpResponse)]).AsType<ILoopServerWebSocket>;

    Result := True;
  end else begin
    Result := False;
  end;
end;

end.

