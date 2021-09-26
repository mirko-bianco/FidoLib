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

unit Fido.Collections.DeepObservableList;

interface

uses
  System.TypInfo,
  System.SysUtils,
  System.Generics.Defaults,
  Spring.Collections,
  Spring.Collections.Lists,
  Fido.DesignPatterns.Observable.Intf,
  Fido.DesignPatterns.Observer.Intf,
  Fido.DesignPatterns.Observer.Notification.Intf,
  Fido.Collections.DeepObservableList.Intf;

type
  TDeepObservableList<T: IObservable> = class(TList<T>, IDeepObservableList<T>, IList<T>, IObserver)
  protected
    procedure Notify(const Sender: IInterface; const Notification: INotification); virtual;
  public
    function Remove(const Item: T): Boolean; override;
    function Extract(const Item: T): T; override;
    procedure Insert(Index: Integer; const Item: T); override;
  end;

implementation

{ TDeepObservableList<T> }

function TDeepObservableList<T>.Extract(const Item: T): T;
var
  Observable: IObservable;
begin
  inherited;
  if Supports(Item, IObservable, Observable) then
    Observable.UnregisterObserver(Self as IObserver);
end;

procedure TDeepObservableList<T>.Insert(Index: Integer; const Item: T);
var
  Observable: IObservable;
begin
  inherited;
  if Supports(Item, IObservable, Observable) then
    Observable.RegisterObserver(Self as IObserver);
end;

procedure TDeepObservableList<T>.Notify(const Sender: IInterface; const Notification: INotification);
var
  Item: T;
begin
  if Supports(Notification.GetSender, GetTypeData(TypeInfo(T)).Guid, Item) then
    Changed(Item, caChanged);
end;

function TDeepObservableList<T>.Remove(const Item: T): Boolean;
var
  Observable: IObservable;
begin
  inherited;
  if Supports(Item, IObservable, Observable) then
    Observable.UnregisterObserver(Self as IObserver);
end;

end.
