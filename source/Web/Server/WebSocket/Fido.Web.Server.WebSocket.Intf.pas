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

unit Fido.Web.Server.WebSocket.Intf;

interface

uses
  System.SysUtils,

  Spring,
  Spring.Collections,

  Fido.Web.Server.WebSocket.Client,
  Fido.Web.Server.WebSocket.Clients;

type
  TWebSocketOnReceivedData = reference to procedure(const Client: TWebSocketClient; const Params: array of TNamedValue);

  IWebSocketServer = interface(IInvokable)
    ['{4D92550D-945B-4CA3-A39C-C67BA98FFF0C}']

    function Clients: TClients;

    procedure Send(const Topic: string; const Message: string); overload;
    procedure Send(const Topic: string; const Data: TArray<Byte>); overload;

    procedure RegisterTopic(const Topic: string; const OnReceivedData: TWebSocketOnReceivedData = nil);

    function Started: Boolean;
    procedure Start;
    procedure Stop;
  end;

  IWebSocketServer<T> = interface(IInvokable)
    ['{3A1ECA22-042A-4532-AC0B-D582BFB1482B}']

    procedure Send(const Topic: string; const Item: T); overload;
  end;

implementation

end.
