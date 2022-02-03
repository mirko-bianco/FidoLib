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

 unit Fido.Gui.Vcl.Action.Anon;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  System.AnsiStrings,
  Vcl.ActnList,
  Vcl.Controls,

  Fido.Exceptions,
  Fido.Gui.Types,
  Fido.Gui.Vcl.NotifyEvent.Delegated;

type
  EAnonAction = class(EFidoException);

  AnonAction = class
    class procedure Setup(const Owner: TComponent; const Control: TControl; const OnExecuteProc: TProc<TObject>; const OnExecuteProcOriginalEventExecutionType: TOriginalEventExecutionType = oeetBefore;
      const OnChangeProc: TProc<TObject> = nil; const OnChangeProcOriginalEventExecutionType: TOriginalEventExecutionType = oeetBefore; const OnStateChangeProc: TProc<TObject> = nil; const OnStateChangeProcOriginalEventExecutionType: TOriginalEventExecutionType = oeetBefore; const OnUpdateProc: TProc<TObject> = nil; const OnUpdateProcOriginalEventExecutionType: TOriginalEventExecutionType = oeetBefore); overload; static;

    class procedure Setup<T>(const Owner: TComponent; const Control: TControl; const Executer: T; const ExecuterMethodName: string;
      const OnExecuteProcOriginalEventExecutionType: TOriginalEventExecutionType = oeetBefore); overload; static;
  end;

implementation

{ AnonAction }

class procedure AnonAction.Setup(
  const Owner: TComponent;
  const Control: TControl;
  const OnExecuteProc: TProc<TObject>;
  const OnExecuteProcOriginalEventExecutionType: TOriginalEventExecutionType;
  const OnChangeProc: TProc<TObject>;
  const OnChangeProcOriginalEventExecutionType: TOriginalEventExecutionType;
  const OnStateChangeProc: TProc<TObject>;
  const OnStateChangeProcOriginalEventExecutionType: TOriginalEventExecutionType;
  const OnUpdateProc: TProc<TObject>;
  const OnUpdateProcOriginalEventExecutionType: TOriginalEventExecutionType);
var
  Action: TAction;
  Buffer: array[0..255] of Char;
  Size: Byte;
begin
  Action := TAction.Create(Owner);

  Size := Control.GetTextLen;
  Inc(Size);
  Control.GetTextBuf(Buffer, Size);
  Action.Caption := Buffer;

  if Assigned(OnExecuteProc) then
    DelegatedNotifyEvent.Setup(Action, Control, 'OnExecute', OnExecuteProc, OnExecuteProcOriginalEventExecutionType);
  if Assigned(OnChangeProc) then
    DelegatedNotifyEvent.Setup(Action, Control, 'OnChange', OnChangeProc, OnChangeProcOriginalEventExecutionType);
  if Assigned(OnStateChangeProc) then
    DelegatedNotifyEvent.Setup(Action, Control, 'OnStateChange', OnStateChangeProc, OnStateChangeProcOriginalEventExecutionType);
  if Assigned(OnUpdateProc) then
    DelegatedNotifyEvent.Setup(Action, Control, 'OnUpdate', OnUpdateProc, OnUpdateProcOriginalEventExecutionType);
  Control.Action := Action;
end;

class procedure AnonAction.Setup<T>(
  const Owner: TComponent;
  const Control: TControl;
  const Executer: T;
  const ExecuterMethodName: string;
  const OnExecuteProcOriginalEventExecutionType: TOriginalEventExecutionType);
var
  Context: TRttiContext;
  Method: TRttiMethod;
begin
  Method := Context.GetType(TypeInfo(T)).GetMethod(ExecuterMethodName);
  if not Assigned(Method) then
    raise EAnonAction.CreateFmt('Method "%s.%s" does not exists.', [Control.Name, ExecuterMethodName]);
  if Method.MethodKind <> mkProcedure then
    raise EAnonAction.CreateFmt('Method "%s.%s" must be a procedure.', [Control.Name, ExecuterMethodName]);
  if Length(Method.GetParameters) <> 0 then
    raise EAnonAction.CreateFmt('Method "%s.%s" must be a parameterless procedure.', [Control.Name, ExecuterMethodName]);

  Setup(
    Owner,
    Control,
    procedure(Sender: TObject)
    begin
      Method.Invoke(TValue.From<T>(Executer), []);
    end,
    OnExecuteProcOriginalEventExecutionType);
end;

end.
