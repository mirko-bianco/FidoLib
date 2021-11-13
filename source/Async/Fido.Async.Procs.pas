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

  Fido.Boxes;

type

  TAsyncProcStatus = (NotStarted, Running, Expired, Failed, Finished);

  TAsyncProcAction = reference to procedure;

  TAsyncProcCatch = reference to procedure(const E: Exception);
  TAsyncProcWhenExpired = TAsyncProcAction;

  IAsyncProc = interface(IInvokable)
    ['{D78C8702-41D4-4821-AD15-EF3B16A4A63B}']

    function &Then(const Action: TAsyncProcAction): IAsyncProc;
    function Catch(const OnCatch: TAsyncProcCatch): IAsyncProc;
    function Within(const SpanInMs: Cardinal; const WhenExpired: TAsyncProcWhenExpired): IAsyncProc;

    function Run: IAsyncProc;

    function Resolve: TAsyncProcStatus;

    function Task: ITask;
  end;

  AsyncProcs = record
  private type
    TAsyncProc = class(TInterfacedObject, IAsyncProc)
    private
      FActions: IQueue<TAsyncProcAction>;
      FSpanInMs: Cardinal;
      FWhenExpired: TAsyncProcWhenExpired;
      FCatch: TAsyncProcCatch;
      FWorker: ITask;
      FTask: ITask;
      FStatus: IBox<TAsyncProcStatus>;
    public
      constructor Create(const Action: TAsyncProcAction);
      destructor Destroy; override;

      function &Then(const Action: TAsyncProcAction): IAsyncProc;
      function Catch(const OnCatch: TAsyncProcCatch): IAsyncProc;
      function Within(const SpanInMs: Cardinal; const WhenExpired: TAsyncProcWhenExpired): IAsyncProc;

      function Run: IAsyncProc;

      function Resolve: TAsyncProcStatus;

      function Task: ITask;
    end;
  public
    class function Queue(const Action: TAsyncProcAction): IAsyncProc; static;
  end;

implementation

{ AsyncProcs.TAsyncProc }

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
      raise Exception.Create(E.Message);
    end;
  FWhenExpired :=
    procedure
    begin
    end;
  FWorker := nil;
  FTask := nil;
  FStatus := Box<TAsyncProcStatus>.Setup(NotStarted);
end;

destructor AsyncProcs.TAsyncProc.Destroy;
begin

  inherited;
end;

function AsyncProcs.TAsyncProc.Resolve: TAsyncProcStatus;
begin
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
      LException: TObject;
      AsyncProc: Weak<IAsyncProc>;
    begin
      AsyncProc := Parent;
      LException := nil;
      try
        while AsyncProc.IsAlive and FActions.TryExtract(Action) and (FStatus.Value <> Expired) do
          Action();

        if not AsyncProc.IsAlive then
          Exit;

        if FStatus.Value <> Expired then
          FStatus.UpdateValue(Finished);
      except
        on E: Exception do
        begin
          try
            FCatch(E);
          except
            LException := AcquireExceptionObject;
          end;

          if Assigned(LException) then
          begin
            FStatus.UpdateValue(Failed);
            TThread.ForceQueue(
              nil,
              procedure
              begin
                raise Exception.Create(LException.ToString);
              end);
            LException.Free;
          end
          else
            FStatus.UpdateValue(Finished);
        end;
      end;
    end);
  FStatus.UpdateValue(Running);

  FTask := TTask.Create(
    procedure
    begin
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