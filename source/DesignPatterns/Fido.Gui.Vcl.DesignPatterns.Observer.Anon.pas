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

unit Fido.Gui.Vcl.DesignPatterns.Observer.Anon;

interface

uses
  System.SysUtils,
  System.Classes,

  Vcl.Forms,

  Spring,
  Spring.Collections,

  Fido.Utilities,
  Fido.DesignPatterns.Observer.Intf,
  Fido.DesignPatterns.Observer.Notification.Intf,
  Fido.Gui.Types,
  Fido.Gui.Vcl.NotifyEvent.Delegated;

type
  TAnonObserver<T: class> = class(TInterfacedObject, IObserver)
  strict private class var
    FObservers: IList<Iobserver>;
  strict private
    FTarget: Weak<T>;
    FNotifyProc: TProc<T, INotification>;
  public
    class constructor Create;
    class destructor Destroy;

    constructor Create(const Target: T; const NotifyProc: TProc<T, INotification>; const Form: TForm = nil);

    // IObserver
    procedure Notify(const Sender: IInterface; const Notification: INotification);
  end;

  TAnonSyncObserver<T: class> = class(TAnonObserver<T>, ISyncObserver);

implementation

{ TAnonObserver<T> }

constructor TAnonObserver<T>.Create(
  const Target: T;
  const NotifyProc: TProc<T, INotification>;
  const Form: TForm);
var
  LForm: TForm;
begin
  FTarget := Utilities.CheckNotNullAndSet<T>(Target, 'Target');
  FNotifyProc := Utilities.CheckNotNullAndSet<TProc<T, INotification>>(NotifyProc, 'NotifyProc');

  FObservers.Add(Self);

  if (Target as TObject).InheritsFrom(TComponent) and
     (Target as TComponent).Owner.InheritsFrom(TForm) then
    LForm := (Target as TComponent).Owner as TForm
  else
    LForm := Form;

  if Assigned(LForm) then
    DelegatedNotifyEvent.Setup(
      LForm,
      LForm,
      'OnDestroy',
      procedure(Sender: TObject)
      begin
        FObservers.Remove(Self);
        Self := nil;
      end,
      oeetAfter);

end;

class destructor TAnonObserver<T>.Destroy;
begin
  FObservers := nil;
end;

class constructor TAnonObserver<T>.Create;
begin
  FObservers := TCollections.CreateList<Iobserver>;
end;

procedure TAnonObserver<T>.Notify(
  const Sender: IInterface;
  const Notification: INotification);
var
  Target: T;
begin
  if FTarget.TryGetTarget(Target) then
    FNotifyProc(Target, Notification);
end;

end.
