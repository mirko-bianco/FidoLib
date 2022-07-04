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

unit Fido.Web.Client.WebSocket.Intf;

interface

uses
  System.Classes,
  System.SysUtils;

type
  TWebSocketOnReceivedData = reference to procedure(const Data: TArray<Byte>);
  TWebSocketOnReceivedMessage = reference to procedure(const Message: string);
  TWebSocketOnReceivedItem<T> = reference to procedure(const Item: T);
  TWebSocketOnError = reference to procedure(const Exception: Exception; var Disconnect: Boolean);

  IWebSocketClient = interface(IInvokable)
    ['{1B2828E1-45E5-4D3F-A479-8EBC7A1E6B7B}']

    function Get(const URL: string; const CustomHeaders: TStrings): string;

    procedure Send(const Message: string); overload;
    procedure Send(const Data: TArray<Byte>); overload;

    function Started: Boolean;
    procedure Start(const URL: string; const CustomHeaders: TStrings; const OnReceivedData: TWebSocketOnReceivedData; const OnError: TWebSocketOnError = nil; const WebsocketProtocol: string = ''); overload;
    procedure Start(const URL: string; const CustomHeaders: TStrings; const OnReceivedMessage: TWebSocketOnReceivedMessage; const OnError: TWebSocketOnError = nil; const WebsocketProtocol: string = ''); overload;
    procedure Stop;
  end;

  IWebSocketClient<T> = interface(IWebSocketClient)
    ['{EE91AAC8-D85F-40BA-87FE-8A6DC727EC4D}']

    procedure Send(const Item: T); overload;
    procedure Start(const URL: string; const CustomHeaders: TStrings; const OnReceivedItem: TWebSocketOnReceivedItem<T>; const OnError: TWebSocketOnError = nil; const WebsocketProtocol: string = ''); overload;
  end;

implementation

end.
