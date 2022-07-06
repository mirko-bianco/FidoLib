(*
 * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without Apiriction, including without limitation the rights
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

unit Fido.Async.Procs;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,

  Spring,
  Spring.Collections,

  Fido.Exceptions,
  Fido.Boxes;

type
  EAsyncProcs = class(EFidoException);

  TAsyncProcStatus = (NotStarted, Running, Expired, Failed, Finished);

  TAsyncProcAction = reference to procedure;

  TAsyncProcCatch = reference to procedure(const E: Exception);
  TAsyncProcWhenExpired = TAsyncProcAction;
  TAsyncProcFinally = reference to procedure;

  IAsyncProc = interface(IInvokable)
    ['{D78C8702-41D4-4821-AD15-EF3B16A4A63B}']

    function &Then(const Action: TAsyncProcAction): IAsyncProc;
    function Catch(const OnCatch: TAsyncProcCatch): IAsyncProc;
    function Within(const SpanInMs: Cardinal; const WhenExpired: TAsyncProcWhenExpired): IAsyncProc;
    function &Finally(const OnFinally: TAsyncProcFinally): IAsyncProc;

    function Run: IAsyncProc;

    function Resolve: TAsyncProcStatus;

    function Task: ITask;
  end deprecated 'Please use Context<T>';

  AsyncProcs = record
  private type
    TAsyncProc = class(TInterfacedObject, IAsyncProc)
    private
      FActions: IQueue<TAsyncProcAction>;
      FSpanInMs: Cardinal;
      FWhenExpired: TAsyncProcWhenExpired;
      FCatch: TAsyncProcCatch;
      FFinally: TAsyncProcFinally;
      FWorker: ITask;
      FTask: ITask;
      FStatus: IBox<TAsyncProcStatus>;
      FResolving: IBox<Boolean>;
    public
      constructor Create(const Action: TAsyncProcAction);
      destructor Destroy; override;

      function &Then(const Action: TAsyncProcAction): IAsyncProc;
      function Catch(const OnCatch: TAsyncProcCatch): IAsyncProc;
      function Within(const SpanInMs: Cardinal; const WhenExpired: TAsyncProcWhenExpired): IAsyncProc;
      function &Finally(const OnFinally: TAsyncProcFinally): IAsyncProc;

      function Run: IAsyncProc;

      function Resolve: TAsyncProcStatus;

      function Task: ITask;
    end;
  public
    class function Queue(const Action: TAsyncProcAction): IAsyncProc; static;
  end deprecated 'Please use Context<T>';

implementation

{ AsyncProcs.TAsyncProc }

function AsyncProcs.TAsyncProc.&Finally(const OnFinally: TAsyncProcFinally): IAsyncProc;
begin
  FFinally := OnFinally;

  Result := Self;
end;

function AsyncProcs.TAsyncProc.Catch(const OnCatch: TAsyncProcCatch): IAsyncProc;
begin
  FStatus.UpdateValue(Failed);
  FCatch := OnCatch;

  Result := Self;
end;

constructor AsyncProcs.TAsyncProc.Create(const Action: TAsyncProcAction);
begin
  inherited Create;

  FActions := TCollections.CreateQueue<TAsyncProcAction>([Action]);
  FSpanInMs := INFINITE;
  FCatch :=
    procedure(const E: Exception)
    begin
      raise EAsyncProcs.Create(E.Message);
    end;
  FFinally :=
    procedure
    begin
    end;
  FWorker := nil;
  FTask := nil;
  FStatus := Box<TAsyncProcStatus>.Setup(NotStarted);
  FResolving := Box<Boolean>.Setup(False);
end;

destructor AsyncProcs.TAsyncProc.Destroy;
begin

  inherited;
end;

function AsyncProcs.TAsyncProc.Resolve: TAsyncProcStatus;
begin
  FResolving.UpdateValue(True);
  while FStatus.Value = Running do
    Sleep(5);

  Result := FStatus.Value;
end;

function AsyncProcs.TAsyncProc.Run: IAsyncProc;
var
  Executed: Boolean;
  Parent: Weak<IAsyncProc>;
begin
  Result := Self;
  Parent := Self;

  FWorker := TTask.Create(
    procedure
    var
      Action: TAsyncProcAction;
      AsyncProc: IAsyncProc;
      ErrorMessage: string;
    begin
      AsyncProc := Parent;
      try
        try
          while FActions.TryExtract(Action) and (FStatus.Value <> Expired) do
            Action();

          if FStatus.Value <> Expired then
            FStatus.UpdateValue(Finished);
        except
          on E: Exception do
          begin
            try
              FCatch(E);
              FStatus.UpdateValue(Finished);
            except
              on E2: Exception do
              begin
                FStatus.UpdateValue(Failed);
                ErrorMessage := E2.Message;
                if not FResolving.Value then
                  TThread.ForceQueue(
                    nil,
                    procedure
                    begin
                      raise EAsyncProcs.Create(ErrorMessage);
                    end);
              end;
            end;
          end;
        end;
      finally
        FFinally();
      end;
    end);
  FStatus.UpdateValue(Running);

  FTask := TTask.Create(
    procedure
    var
      AsyncProc: IAsyncProc;
    begin
      AsyncProc := Parent;
      Executed := FWorker.Start.Wait(FSpanInMs);
      if (not Executed) and (FStatus.Value = Running) then
      begin
        FStatus.UpdateValue(Expired);
        FWhenExpired();
      end;
    end).Start;
end;

function AsyncProcs.TAsyncProc.Task: ITask;
begin
  Result := FTask;
end;

function AsyncProcs.TAsyncProc.&Then(const Action: TAsyncProcAction): IAsyncProc;
begin
  FActions.Enqueue(Action);

  Result := Self;
end;

function AsyncProcs.TAsyncProc.Within(
  const SpanInMs: Cardinal;
  const WhenExpired: TAsyncProcWhenExpired): IAsyncProc;
begin
  FSpanInMs := SpanInMs;
  FWhenExpired := WhenExpired;

  Result := Self;
end;

{ AsyncProcs }

class function AsyncProcs.Queue(const Action: TAsyncProcAction): IAsyncProc;
begin
  Result := AsyncProcs.TAsyncProc.Create(Action);
end;

end.
