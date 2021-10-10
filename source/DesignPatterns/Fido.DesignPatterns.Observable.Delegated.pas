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

unit Fido.DesignPatterns.Observable.Delegated;

interface

uses
  System.Classes,
  System.SysUtils,
  Spring,
  Spring.Collections,

  Fido.DesignPatterns.Observer.Intf,
  Fido.DesignPatterns.Observable.Intf,
  Fido.DesignPatterns.Observer.Notification.Intf,
  Fido.DesignPatterns.Observer.Notification,
  Fido.NamedObject.Intf;

type
  TDelegatedObservable = class(TInterfacedObject, IObservable)
  strict private
    FLock: IReadWriteSync;
    FIdentity: string;
    FObservable: Weak<IInterface>;
    FNeedsSync: boolean;
    FObservers: IList<Weak<IObserver>>;
    FPaused: boolean;

    function GetObservable: IInterface;
    procedure ClearObservers;
    procedure Relay(const Sender: IInterface; const Notification: INotification);
  public
    constructor Create(const Observable: IInterface; const Identity: string = '');
    destructor Destroy; override;
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
  end;

implementation

{ TSynchronizedNotifcationThread }

type
  TSynchronizedNotificationThread = class(TThread)
  private
    FProc: TProc;

    procedure ExecProc;
  protected
    procedure Execute; override;
  public
    constructor Create(Proc: TProc);
  end;

constructor TSynchronizedNotificationThread.Create(Proc: TProc);
begin
  Guard.CheckTrue(Assigned(Proc), 'Proc');
  Assert(Assigned(Proc));
  inherited Create;
  FProc := Proc;
  FreeOnTerminate := True;
end;

procedure TSynchronizedNotificationThread.ExecProc;
begin
  FProc();
end;

procedure TSynchronizedNotificationThread.Execute;
begin
  inherited;
  Synchronize(ExecProc);
end;

{ TDelegatedObservable }

procedure TDelegatedObservable.Broadcast(const Notification: INotification);
begin
  Relay(GetObservable, Notification);
end;

procedure TDelegatedObservable.Broadcast(
  const Description: string;
  const Data: TNotificationData);
begin
  if IsPaused then
    Exit;

  Broadcast(TNotification.Create(GetObservable, Description, Data));
end;

procedure TDelegatedObservable.Broadcast(const Description: string);
begin
  if Description <> '' then
    Broadcast(Description, TValue.Empty);
end;

procedure TDelegatedObservable.ClearObservers;
begin
  FLock.BeginWrite;
  try
    FObservers.Clear;
  finally
    FLock.EndWrite;
  end;
end;

constructor TDelegatedObservable.Create(
  const Observable: IInterface;
  const Identity: string = '');
var
  Named: INamedObject;
  Name: string;
begin
  inherited Create;
  FLock := TMREWSync.Create;
  FObservers := TCollections.CreateList<Weak<IObserver>>;
  if not Assigned(Observable) then
    FObservable := Weak<IInterface>.Create(Self)
  else
    FObservable := Weak<IInterface>.Create(Observable);
  FIdentity := Identity;

  // don't try to generate identity if 1) we already got one, 2) it could only be based on Self
  if (FIdentity <> '') then
    Exit;

  if Supports(Observable, INamedObject, Named) then
  begin
    Name := Named.GetDescription;
    if Name <> '' then
      Name := '[' + Name +']';
  end
  else
    Name := '';

  FIdentity := Copy((FObservable.Target as TObject).ClassName, 2, 100) + Name;
end;

destructor TDelegatedObservable.Destroy;
begin
  Pause;
  ClearObservers;
  inherited;
end;

function TDelegatedObservable.GetIdentity: string;
begin
  Result := FIdentity;
end;

function TDelegatedObservable.GetObservable: IInterface;
begin
  if not FObservable.TryGetTarget(Result) then
    Result := Self;
end;

function TDelegatedObservable.IsPaused: boolean;
begin
  FLock.BeginRead;
  try
    Result := FPaused;
  finally
    FLock.EndRead;
  end;
end;

procedure TDelegatedObservable.Pause;
begin
  if IsPaused then
    Exit;

  FLock.BeginWrite;
  try
    FPaused := true;
  finally
    FLock.EndWrite;
  end;
end;

procedure TDelegatedObservable.RegisterObserver(const Observer: IObserver);
begin
  FLock.BeginWrite;
  try
    // cannot use direct IndexOf because we hold Weaks
    with FObservers.GetEnumerator do
      while MoveNext do
        if Current.Target = Observer then
          Exit;

    FObservers.Add(Weak<IObserver>.Create(Observer));
    FNeedsSync := FNeedsSync or Supports(Observer, IGUIObserver);
  finally
    FLock.EndWrite;
  end;
end;

procedure TDelegatedObservable.Relay(
  const Sender: IInterface;
  const Notification: INotification);
var
  ProcEnum: TProc;
  List: TArray<Weak<IObserver>>;
begin
  if IsPaused then
    Exit;

  // make a copy of the list because Observers may want to unregister upon
  // notification, which actually breaks enumeration. Cannot use IReadOnlyList
  // because internally it still references the original one
  FLock.BeginRead;
  try
    List := FObservers.ToArray;
  finally
    FLock.EndRead;
  end;

  ProcEnum :=
    procedure
    var
      Observer: Weak<IObserver>;
    begin
      for Observer in List do
        if Observer.IsAlive then
          Observer.Target.Notify(Sender, Notification);
    end;

  if FNeedsSync then
    TSynchronizedNotificationThread.Create(ProcEnum)
  else
    ProcEnum;
end;

procedure TDelegatedObservable.Resume(const AndBroadcast: string);
begin
  if IsPaused then
  begin
    FLock.BeginWrite;
    try
      FPaused := false;
    finally
      FLock.EndWrite;
    end;
  end;

  Broadcast(AndBroadcast);
end;

procedure TDelegatedObservable.UnregisterObserver(const Observer: IObserver);
var
  I : integer;
begin
  FLock.BeginWrite;
  try
    // do 3 things in one go: remove this one along with dead observers and recheck for VCL
    FNeedsSync := false;

    // loop because we're removing items
    for I := FObservers.Count -1 downto 0 do
      with FObservers[I] do
        if not IsAlive or (Target = Observer) then
          FObservers.Delete(I)
        else
          FNeedsSync := FNeedsSync or Supports(Target, IGUIObserver);
  finally
    FLock.EndWrite;
  end;
end;

end.
