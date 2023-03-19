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

  Spring,
  Spring.Collections,
  Spring.Collections.Lists,

  Fido.DesignPatterns.Observable.Intf,
  Fido.DesignPatterns.Observable.Delegated,
  Fido.DesignPatterns.Observer.Intf,
  Fido.DesignPatterns.Observer.Notification.Intf,
  Fido.Collections.DeepObservableList.Intf;

type
  TDeepObservableList<T: IObservable> = class(TList<T>, IDeepObservableList<T>, IList<T>, IObserver, IObservable)
  private
    FDelegatedObservable: IObservable;
  protected
    // IObserver
    procedure Notify(const Sender: IInterface; const Notification: INotification); overload;
    // IObservable
    procedure RegisterObserver(const Observer: IObserver);
    procedure UnregisterObserver(const Observer: IObserver);
    function GetIdentity: string;
    procedure Broadcast(const Notification: INotification); overload;
    procedure Broadcast(const Description: string); overload;
    procedure Broadcast(const Description: string; const Data: TNotificationData); overload;
    function IsPaused: boolean;
    procedure Pause;
    procedure Resume(const AndBroadcast: string = '');
  public const
    ITEM_ADDED = 'Item added';
    ITEMS_ADDED = 'Items added';
    ITEMS_CLEARED = 'Items cleared';
    ITEM_DELETED = 'Item deleted';
    ITEMS_DELETED = 'Items deleted';
    ITEM_EXTRACTED = 'Item extracted';
    ITEMS_EXTRACTED = 'Items extracted';
    ITEM_INSERTED = 'Item inserted';
    ITEM_REMOVED = 'Item removed';
    ITEMS_INSERTED = 'Items inserted';
    ITEMS_REMOVED = 'Items removed';
  public
    constructor Create; overload;

   {$REGION ' ICollection<T> '}
    procedure AddRange(const values: array of T); overload;
    procedure AddRange(const values: IEnumerable<T>); overload;

    procedure Clear;

    function Remove(const item: T): Boolean;

    function RemoveAll(const predicate: Predicate<T>): Integer;

    function RemoveRange(const values: array of T): Integer; overload;
    function RemoveRange(const values: IEnumerable<T>): Integer; overload;

    function Extract(const item: T): T;
    function ExtractAll(const predicate: Predicate<T>): TArray<T>;
    procedure ExtractRange(const values: array of T); overload;
    procedure ExtractRange(const values: IEnumerable<T>); overload;
   {$ENDREGION}

   {$REGION ' IList<T> '}
    function Add(const item: T): Integer; overload;
    procedure Insert(index: Integer; const item: T);
    procedure InsertRange(index: Integer; const values: array of T); overload;
    procedure InsertRange(index: Integer; const values: IEnumerable<T>); overload;

    procedure Delete(index: Integer);
    procedure DeleteRange(index, count: Integer);

    function ExtractAt(index: Integer): T;
    function ExtractRange(index, count: Integer): TArray<T>; overload;
   {$ENDREGION}
  end;

implementation

{ TDeepObservableList<T> }

procedure TDeepObservableList<T>.Broadcast(const Notification: INotification);
begin
  FDelegatedObservable.Broadcast(Notification);
end;

procedure TDeepObservableList<T>.Broadcast(const Description: string);
begin
  FDelegatedObservable.Broadcast(Description);
end;

function TDeepObservableList<T>.Add(const item: T): Integer;
var
  Observable: IObservable;
begin
  inherited;
  if Supports(Item, IObservable, Observable) then
    Observable.RegisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEM_ADDED);
end;

procedure TDeepObservableList<T>.AddRange(const values: IEnumerable<T>);
var
  Observable: IObservable;
begin
  inherited;

  with values.GetEnumerator do
    while MoveNext do
      if Supports(GetCurrent, IObservable, Observable) then
        Observable.RegisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEMS_ADDED);
end;

procedure TDeepObservableList<T>.AddRange(const values: array of T);
var
  Observable: IObservable;
begin
  inherited;

  for var Item in values do
    if Supports(Item, IObservable, Observable) then
       Observable.RegisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEMS_ADDED);
end;

procedure TDeepObservableList<T>.Broadcast(
  const Description: string;
  const Data: TNotificationData);
begin
  FDelegatedObservable.Broadcast(Description, Data);
end;

procedure TDeepObservableList<T>.Clear;
var
  Observable: IObservable;
begin
  for var Item in ToArray do
    if Supports(Item, IObservable, Observable) then
       Observable.UnregisterObserver(Self);

  inherited;

  FDelegatedObservable.Broadcast(ITEMS_CLEARED);
end;

constructor TDeepObservableList<T>.Create;
begin
  inherited;
  FDelegatedObservable := TDelegatedObservable.Create(Self);
end;

procedure TDeepObservableList<T>.Delete(index: Integer);
var
  Observable: IObservable;
begin
  if Supports(Items[index], IObservable, Observable) then
    Observable.UnregisterObserver(Self);
  inherited;

  FDelegatedObservable.Broadcast(ITEM_DELETED);
end;

procedure TDeepObservableList<T>.DeleteRange(index, count: Integer);
var
  Observable: IObservable;
begin
  for var Item in ToArray do
    if Supports(Item, IObservable, Observable) then
       Observable.UnregisterObserver(Self);

  inherited;

  for var Item in ToArray do
    if Supports(Item, IObservable, Observable) then
       Observable.RegisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEMS_DELETED);
end;

function TDeepObservableList<T>.Extract(const Item: T): T;
var
  Observable: IObservable;
begin
  result := inherited;
  if Supports(Result, IObservable, Observable) then
    Observable.UnregisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEM_EXTRACTED);
end;

function TDeepObservableList<T>.ExtractAll(const predicate: Predicate<T>): TArray<T>;
var
  Observable: IObservable;
begin
  Result := inherited;

  for var Item in Result do
    if Supports(Item, IObservable, Observable) then
       Observable.UnregisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEMS_EXTRACTED);
end;

function TDeepObservableList<T>.ExtractAt(index: Integer): T;
var
  Observable: IObservable;
begin
  Result := inherited;

  if Supports(Result, IObservable, Observable) then
    Observable.UnregisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEM_EXTRACTED);
end;

procedure TDeepObservableList<T>.ExtractRange(const values: array of T);
var
  Observable: IObservable;
begin
  inherited;

  for var Item in Values do
    if Supports(Item, IObservable, Observable) then
       Observable.UnregisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEMS_EXTRACTED);
end;

procedure TDeepObservableList<T>.ExtractRange(const values: IEnumerable<T>);
var
  Observable: IObservable;
begin
  inherited;

  for var Item in Values do
    if Supports(Item, IObservable, Observable) then
       Observable.UnregisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEMS_EXTRACTED);
end;

function TDeepObservableList<T>.GetIdentity: string;
begin
  Result := FDelegatedObservable.GetIdentity;
end;

procedure TDeepObservableList<T>.Insert(
  Index: Integer;
  const Item: T);
var
  Observable: IObservable;
  Delegated: IObserver;
begin
  inherited;
  if Supports(Item, IObservable, Observable) then
    Observable.RegisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEM_INSERTED);
end;

procedure TDeepObservableList<T>.InsertRange(index: Integer; const values: IEnumerable<T>);
var
  Observable: IObservable;
begin
  inherited;

  for var Item in Values do
    if Supports(Item, IObservable, Observable) then
       Observable.RegisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEMS_INSERTED);
end;

procedure TDeepObservableList<T>.InsertRange(index: Integer; const values: array of T);
var
  Observable: IObservable;
begin
  inherited;

  for var Item in Values do
    if Supports(Item, IObservable, Observable) then
       Observable.RegisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEMS_REMOVED);
end;

function TDeepObservableList<T>.IsPaused: boolean;
begin
  Result := FDelegatedObservable.IsPaused;
end;

procedure TDeepObservableList<T>.Notify(
  const Sender: IInterface;
  const Notification: INotification);
var
  Item: T;
begin
  if Supports(Notification.GetSender, GetTypeData(TypeInfo(T)).Guid, Item) then
    Changed(Item, caChanged);
  FDelegatedObservable.Broadcast(Notification);
end;

procedure TDeepObservableList<T>.Pause;
begin
  FDelegatedObservable.Pause;
end;

procedure TDeepObservableList<T>.RegisterObserver(const Observer: IObserver);
begin
  FDelegatedObservable.RegisterObserver(Observer);
end;

function TDeepObservableList<T>.Remove(const Item: T): Boolean;
var
  Observable: IObservable;
begin
  inherited;
  if Supports(Item, IObservable, Observable) then
    Observable.UnregisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEM_REMOVED);
end;

function TDeepObservableList<T>.RemoveAll(const predicate: Predicate<T>): Integer;
var
  Observable: IObservable;
begin
  for var Item in ToArray do
    if Supports(Item, IObservable, Observable) then
       Observable.UnregisterObserver(Self);

  Result := inherited;

  for var Item in ToArray do
    if Supports(Item, IObservable, Observable) then
       Observable.RegisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEMS_REMOVED);
end;

function TDeepObservableList<T>.RemoveRange(const values: array of T): Integer;
var
  Observable: IObservable;
begin
  for var Item in ToArray do
    if Supports(Item, IObservable, Observable) then
       Observable.UnregisterObserver(Self);

  Result := inherited;

  for var Item in ToArray do
    if Supports(Item, IObservable, Observable) then
       Observable.RegisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEMS_REMOVED);
end;

function TDeepObservableList<T>.RemoveRange(const values: IEnumerable<T>): Integer;
var
  Observable: IObservable;
begin
  for var Item in Values do
    if Supports(Item, IObservable, Observable) then
       Observable.UnregisterObserver(Self);

  Result := inherited;

  FDelegatedObservable.Broadcast(ITEMS_REMOVED);
end;

procedure TDeepObservableList<T>.Resume(const AndBroadcast: string);
begin
  FDelegatedObservable.Resume(AndBroadcast);
end;

procedure TDeepObservableList<T>.UnregisterObserver(const Observer: IObserver);
begin
  FDelegatedObservable.UnregisterObserver(Observer);
end;

function TDeepObservableList<T>.ExtractRange(index, count: Integer): TArray<T>;
var
  Observable: IObservable;
begin
  Result := inherited;

  for var Item in Result do
    if Supports(Item, IObservable, Observable) then
       Observable.RegisterObserver(Self);

  FDelegatedObservable.Broadcast(ITEMS_EXTRACTED);
end;

end.
