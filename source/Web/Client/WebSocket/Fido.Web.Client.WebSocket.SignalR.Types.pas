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

unit Fido.Web.Client.WebSocket.SignalR.Types;

interface

uses
  Spring;

type
  INegotiationResponse = interface(IInvokable)
    ['{628E4BFB-A477-40F0-929D-6D51E79779B4}']

    function Url: string;
    function ConnectionToken: string;
    function ConnectionId: string;
    function KeepAliveTimeout: Extended;
    function DisconnectionTimeout: Extended;
    function ConnectionTimeout: Extended;
    function TryWebSockets: Boolean;
    function ProtocolVersion: string;
    function TransportConnectTimeout: Extended;
    function LongPollDelay: Extended;
  end;

  IChannelResult = interface(IInvokable)
    ['{0C5ED61E-965F-4EE1-BB4C-2DC025742AF8}']

    function Success: Boolean;
    function ErrorCode: Nullable<string>;
  end;

  TSignalRMethodResult = record
  private
    FSuccess: Boolean;
    FErrorMessage: string;
  public
    constructor Create(const Success: Boolean; const ErrorMessage: string);

    function Success: Boolean;
    function ErrorMessage: string;
  end;

implementation

{ TSignalRMethodResult }

constructor TSignalRMethodResult.Create(const Success: Boolean; const ErrorMessage: string);
begin
  FSuccess := Success;
  FErrorMessage := ErrorMessage;
end;

function TSignalRMethodResult.ErrorMessage: string;
begin
  Result := FErrorMessage;
end;

function TSignalRMethodResult.Success: Boolean;
begin
  Result := FSuccess;
end;

end.

