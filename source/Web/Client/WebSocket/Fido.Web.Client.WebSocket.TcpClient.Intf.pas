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

unit Fido.Web.Client.WebSocket.TcpClient.Intf;

interface

uses
  System.Classes,

  Fido.Exceptions,
  Fido.Web.Client.WebSocket.Intf;

type
  EWebSocketTCPClient = class(EFidoException);

  IWebSocketTCPClient = interface(IInvokable)
    ['{C1D03C21-23C1-4A32-B715-CDED9FEDCC05}']

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

end.
