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

unit Fido.DesignPatterns.Observer.Notification;

interface

uses
  Fido.DesignPatterns.Observer.Notification.Intf;

type
  TNotification = class(TInterfacedObject, INotification)
  strict private
    FData: TNotificationData;
    FDescription: string;
    FSender: IInterface;
  public
    constructor Create(const Sender: IInterface; const Description: string; const Data: TNotificationData);

    function GetData: TNotificationData;
    function GetDescription: string;
    function GetSender: IInterface;
  end;

implementation

{ TNotification }

constructor TNotification.Create(
  const Sender: IInterface;
  const Description: string;
  const Data: TNotificationData);
begin
  inherited Create;
  FSender := Sender;
  FData := Data;
  FDescription := Description;
end;

function TNotification.GetData: TNotificationData;
begin
  Result := FData;
end;

function TNotification.GetDescription: string;
begin
  Result := FDescription;
end;

function TNotification.GetSender: IInterface;
begin
  Result := FSender;
end;

end.
