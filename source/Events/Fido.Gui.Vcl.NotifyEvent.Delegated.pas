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

unit Fido.Gui.Vcl.NotifyEvent.Delegated;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.Classes,
  System.Sysutils,

  VCL.Controls,
  VCL.ActnList,

  Spring,

  Fido.Exceptions,
  Fido.Gui.Types;

type
  EDelegatedNotifyEvent = class(EFidoException);

  DelegatedNotifyEvent = class
  private type
    TNotifyEventWrapper = class(TComponent)
    private
      FProc: TProc<TObject>;
    public
      constructor Create(const Owner: TComponent; const Proc: TProc<TObject>); reintroduce;
    published
      procedure OnEvent(Sender: TObject);
    end;
  protected
    class function Generate(const Owner: TComponent; const Control: TControl; const Proc: TProc<TObject>; const OriginalEvent: TNotifyEvent = nil;
      const OriginalEventExecutionType: TOriginalEventExecutionType = oeetBefore): TNotifyEvent; static;
  public
    class procedure Setup(const Owner: TComponent; const Control: TControl; const EventName: string; const Proc: TProc<TObject>;
      const OriginalEventExecutionType: TOriginalEventExecutionType = oeetBefore); overload; static;
  end;

implementation

{ DelegatedNotifyEvent.TNotifyEventWrapper }

constructor DelegatedNotifyEvent.TNotifyEventWrapper.Create(
  const Owner: TComponent;
  const Proc: TProc<TObject>);
begin
  inherited Create(Owner);
  FProc := Proc;
end;

procedure DelegatedNotifyEvent.TNotifyEventWrapper.OnEvent(Sender: TObject);
begin
  FProc(Sender);
end;

{ DelegatedNotifyEvent }

class function DelegatedNotifyEvent.Generate(
  const Owner: TComponent;
  const Control: TControl;
  const Proc: TProc<TObject>;
  const OriginalEvent: TNotifyEvent;
  const OriginalEventExecutionType: TOriginalEventExecutionType): TNotifyEvent;
var
  CompositeProc: TProc<TObject>;
  LocalNotifyEvent: TNotifyEvent;
  LocalAction: TBasicAction;
begin
  Guard.CheckTrue(Assigned(Proc), 'Proc is not assigned.');
  LocalNotifyEvent := OriginalEvent;
  LocalAction := Control.Action;
  CompositeProc := procedure(Sender: TObject)
    begin
      if (OriginalEventExecutionType = oeetBefore) and Assigned(LocalNotifyEvent) then
        LocalNotifyEvent(Sender);
      if (OriginalEventExecutionType = oeetBefore) and Assigned(LocalAction) then
        LocalAction.Execute;
      Proc(Sender);
      if (OriginalEventExecutionType = oeetAfter) and Assigned(LocalNotifyEvent) then
        LocalNotifyEvent(Sender);
      if (OriginalEventExecutionType = oeetAfter) and Assigned(LocalAction) then
        LocalAction.Execute;
    end;
  Result := TNotifyEventWrapper.Create(Owner, CompositeProc).OnEvent;
end;

class procedure DelegatedNotifyEvent.Setup(
      const Owner: TComponent;
      const Control: TControl;
      const EventName: string;
      const Proc: TProc<TObject>;
      const OriginalEventExecutionType: TOriginalEventExecutionType = oeetBefore);
var
  Context: TRttiContext;
  LProperty: TRttiProperty;
  MethodRecPtr: ^TMethod;
  Value: TValue;
  OriginalEvent: TNotifyEvent;
  DelegatedEvent: TNotifyEvent;
begin
  LProperty := Context.GetType(Owner.ClassType).GetProperty(EventName);
  if not Assigned(LProperty) then
    raise EDelegatedNotifyEvent.CreateFmt('Property "%s.%s" does not exist', [Owner.Name, EventName]);
  if LProperty.PropertyType.TypeKind <> tkMethod then
    raise EDelegatedNotifyEvent.CreateFmt('Property "%s.%s" is not a method', [Owner.Name, EventName]);

  OriginalEvent := nil;
  Value := LProperty.GetValue(Owner);
  if not Value.IsEmpty then
  begin
    MethodRecPtr := Value.GetReferenceToRawData;
    if not(Value.TypeInfo = TypeInfo(TNotifyEvent)) then
      Exit;

    OriginalEvent := TNotifyEvent(MethodRecPtr^);
  end;

  DelegatedEvent := Generate(Owner, Control, Proc, OriginalEvent, OriginalEventExecutionType);
  LProperty.SetValue(Owner, TValue.From<TNotifyEvent>(DelegatedEvent));
end;

end.

