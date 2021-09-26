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

unit Fido.DesignPatterns.Observable.Intf;

interface

uses
  System.Rtti,

  Spring,

  Fido.DesignPatterns.Observer.Notification.Intf,
  Fido.DesignPatterns.Observer.Intf;

type
  IObservable = interface(IInvokable)
    ['{54BCAF18-3021-439C-B8BC-8D4D30E1ECF4}']
    procedure RegisterObserver(const Observer: IObserver);
    procedure UnregisterObserver(const Observer: IObserver);
    function GetIdentity: string;
    procedure Broadcast(const Notification: INotification); overload;
    procedure Broadcast(const Description: string); overload;
    procedure Broadcast(const Description: string; const Data: TNotificationData); overload;
    function IsPaused: boolean;
    procedure Pause;
    procedure Resume(const AndBroadcast: string = '');
  end;

  Observables = class
    class procedure SetAndBroadcast<T>(const Observable: IObservable; var Member: T; const Value: T; const Notification: INotification); overload; static;
    class procedure SetAndBroadcast<T>(const Observable: IObservable; var Member: T; const Value: T; const Description: string); overload; static;
    class procedure SetAndBroadcast<T>(const Observable: IObservable; var Member: T; const Value: T; const Description: string; const Data: TNotificationData); overload; static;
  end;

implementation

{ Observables }

class procedure Observables.SetAndBroadcast<T>(const Observable: IObservable; var Member: T; const Value: T; const Notification: INotification);
begin
  if TValue.From<T>(Member) <> TValue.From<T>(Value) then
  begin
    Member := Value;
    Observable.Broadcast(Notification);
  end;
end;

class procedure Observables.SetAndBroadcast<T>(const Observable: IObservable; var Member: T; const Value: T; const Description: string);
begin
  if TValue.From<T>(Member) <> TValue.From<T>(Value) then
  begin
    Member := Value;
    Observable.Broadcast(Description);
  end;
end;

class procedure Observables.SetAndBroadcast<T>(const Observable: IObservable; var Member: T; const Value: T; const Description: string; const Data: TNotificationData);
begin
  if TValue.From<T>(Member) <> TValue.From<T>(Value) then
  begin
    Member := Value;
    Observable.Broadcast(Description, Data);
  end;
end;

end.
