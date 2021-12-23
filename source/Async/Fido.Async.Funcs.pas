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

unit Fido.Async.Funcs;

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
  EAsyncFuncs = class(EFidoException);

  TAsyncFuncStatus = (NotStarted, Running, Expired, Failed, Finished);

  TAsyncFuncResult<T> = record
  private
    FStatus: TAsyncFuncStatus;
    FValue: Nullable<T>;
  public
    constructor Create(const Status: TAsyncFuncStatus; const Value: Nullable<T>);

    function Status: TAsyncFuncStatus;
    function Value: Nullable<T>;
  end;

  TAsyncFuncTypedAction<TFrom, TTo> = reference to function(const Value: TFrom): TTo;

  TAsyncFuncAction = reference to function(const Value: TValue): TValue;
  TAsyncFuncCatch<T> = reference to function(const E: Exception): T;
  TAsyncFuncWhenExpired<T> = reference to function: T;
  TAsyncFuncFinally = reference to procedure;

  IAsyncFunc<TFrom, TTo> = interface(IInvokable)
    ['{92292D61-AC9C-4A36-ADBE-D7AEEDC58449}']

    function &Then(const Action: TAsyncFuncAction): IAsyncFunc<TFrom, TTo>;
    function Catch(const OnCatch: TAsyncFuncCatch<TTo>): IAsyncFunc<TFrom, TTo>;
    function Within(const SpanInMs: Cardinal; const WhenExpired: TAsyncFuncWhenExpired<TTo>): IAsyncFunc<TFrom, TTo>;
    function &Finally(const OnFinally: TAsyncFuncFinally): IAsyncFunc<TFrom, TTo>;

    function Run(const Value: TFrom): IAsyncFunc<TFrom, TTo>;

    function Resolve: TAsyncFuncResult<TTo>;

    function Task: ITask;
  end;

  AsyncFuncs<TFrom, TTo> = record
  private type
    TAsyncFunc = class(TInterfacedObject, IAsyncFunc<TFrom, TTo>)
    private
      FActions: IQueue<TAsyncFuncAction>;
      FSpanInMs: Cardinal;
      FWhenExpired: TAsyncFuncWhenExpired<TTo>;
      FCatch: TAsyncFuncCatch<TTo>;
      FFinally: TAsyncFuncFinally;
      FFuture: IFuture<TTo>;
      FTask: ITask;
      FStatus: IBox<TAsyncFuncStatus>;
      FExpiredValue: IBox<TTo>;
      FResolving: IBox<Boolean>;
    public
      constructor Create(const Action: TAsyncFuncAction);

      function &Then(const Action: TAsyncFuncAction): IAsyncFunc<TFrom, TTo>;
      function Catch(const OnCatch: TAsyncFuncCatch<TTo>): IAsyncFunc<TFrom, TTo>;
      function Within(const SpanInMs: Cardinal; const WhenExpired: TAsyncFuncWhenExpired<TTo>): IAsyncFunc<TFrom, TTo>;
      function &Finally(const OnFinally: TAsyncFuncFinally): IAsyncFunc<TFrom, TTo>;

      function Run(const Value: TFrom): IAsyncFunc<TFrom, TTo>;

      function Resolve: TAsyncFuncResult<TTo>;

      function Task: ITask;
    end;
  public
    class function Queue(const Action: TAsyncFuncAction): IAsyncFunc<TFrom, TTo>; static;
  end;

  AsyncFuncMapping = record
    class function Action<TFrom, TTo>(const MappedAction: TAsyncFuncTypedAction<TFrom, TTo>): TAsyncFuncAction; static;
  end;

implementation

{ AsyncFuncs<TFrom, TTo>.TAsyncFunc }

function AsyncFuncs<TFrom, TTo>.TAsyncFunc.&Finally(const OnFinally: TAsyncFuncFinally): IAsyncFunc<TFrom, TTo>;
begin
  FFinally := OnFinally;
  Result := Self;
end;

function AsyncFuncs<TFrom, TTo>.TAsyncFunc.Catch(const OnCatch: TAsyncFuncCatch<TTo>): IAsyncFunc<TFrom, TTo>;
begin
  FStatus.UpdateValue(Failed);
  FCatch := OnCatch;

  Result := Self;
end;

constructor AsyncFuncs<TFrom, TTo>.TAsyncFunc.Create(const Action: TAsyncFuncAction);
begin
  inherited Create;

  FActions := TCollections.CreateQueue<TAsyncFuncAction>([Action]);
  FSpanInMs := INFINITE;
  FCatch :=
    function(const E: Exception): TTo
    begin
      raise E;
    end;
  FWhenExpired :=
    function: TTo
    begin
      Result := Default(TTo);
    end;
  FFinally :=
    procedure
    begin
    end;
  FFuture := nil;
  FTask := nil;
  FStatus := Box<TAsyncFuncStatus>.Setup(NotStarted);
  FExpiredValue := Box<TTo>.Setup(Default(TTo));
  FResolving := Box<Boolean>.Setup(False);
end;

function AsyncFuncs<TFrom, TTo>.TAsyncFunc.Resolve: TAsyncFuncResult<TTo>;
var
  Value: Nullable<TTo>;
  Status: TAsyncFuncStatus;
begin
  FResolving.UpdateValue(True);
  while FStatus.Value = Running do
    Sleep(5);

  Status := FStatus.Value;

  if Status = Finished then
    Value := Nullable<TTo>.Create(FFuture.Value)
  else if Status = Expired then
    Value := Nullable<TTo>.Create(FExpiredValue.Value);

  Result := TAsyncFuncResult<TTo>.Create(Status, Value);
end;

function AsyncFuncs<TFrom, TTo>.TAsyncFunc.Run(const Value: TFrom): IAsyncFunc<TFrom, TTo>;
var
  Executed: Boolean;
  Parent: Weak<IAsyncFunc<TFrom, TTo>>;
begin
  Result := Self;
  Parent := Self;

  FFuture := TTask.Future<TTo>(
    function: TTo
    var
      Action: TAsyncFuncAction;
      CurrValue: TValue;
      AsyncFunc: IAsyncFunc<TFrom, TTo>;
      ErrorMessage: string;
    begin
      AsyncFunc := Parent;
      CurrValue := TValue.From<TFrom>(Value);
      try
        try
          while FActions.TryExtract(Action) and (FStatus.Value <> Expired) do
            CurrValue := Action(CurrValue);

          if FStatus.Value <> Expired then
          begin
            Result := CurrValue.AsType<TTo>;
            FStatus.UpdateValue(Finished);
          end;
        except
          on E: Exception do
          begin
            try
              Result := FCatch(E);
              FStatus.UpdateValue(Finished);
            except
              on E2: Exception do
              begin
                ErrorMessage := E2.Message;
                FStatus.UpdateValue(Failed);
                if not FResolving.Value then
                  TThread.ForceQueue(
                    nil,
                    procedure
                    begin
                      raise EAsyncFuncs.Create(ErrorMessage);
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
      AsyncFunc: IAsyncFunc<TFrom, TTo>;
    begin
      AsyncFunc := Parent;
      FFuture.Start;
      Executed := FFuture.Wait(FSpanInMs);
      if (not Executed) and (FStatus.Value = Running) then
      begin
        FStatus.UpdateValue(Expired);
        FExpiredValue.UpdateValue(FWhenExpired());
      end;
    end).Start;
end;

function AsyncFuncs<TFrom, TTo>.TAsyncFunc.Task: ITask;
begin
  Result := FTask;
end;

function AsyncFuncs<TFrom, TTo>.TAsyncFunc.&Then(const Action: TAsyncFuncAction): IAsyncFunc<TFrom, TTo>;
begin
  FActions.Enqueue(Action);

  Result := Self;
end;

function AsyncFuncs<TFrom, TTo>.TAsyncFunc.Within(
  const SpanInMs: Cardinal;
  const WhenExpired: TAsyncFuncWhenExpired<TTo>): IAsyncFunc<TFrom, TTo>;
begin
  FSpanInMs := SpanInMs;
  FWhenExpired := WhenExpired;

  Result := Self;
end;

{ AsyncFuncMapping }

class function AsyncFuncMapping.Action<TFrom, TTo>(const MappedAction: TAsyncFuncTypedAction<TFrom, TTo>): TAsyncFuncAction;
begin
  Result :=
    function(const Value: TValue): TValue
    begin
      Result := TValue.From<TTo>(MappedAction(Value.AsType<TFrom>));
    end;
end;

{ AsyncFuncs<TFrom, TTo> }

class function AsyncFuncs<TFrom, TTo>.Queue(const Action: TAsyncFuncAction): IAsyncFunc<TFrom, TTo>;
begin
  Result := TAsyncFunc.Create(Action);
end;

{ TAsyncFuncResult<T> }

constructor TAsyncFuncResult<T>.Create(const Status: TAsyncFuncStatus; const Value: Nullable<T>);
begin
  FStatus := Status;
  FValue := Value;
end;

function TAsyncFuncResult<T>.Status: TAsyncFuncStatus;
begin
  Result := FStatus;
end;

function TAsyncFuncResult<T>.Value: Nullable<T>;
begin
  Result := FValue;
end;

end.
