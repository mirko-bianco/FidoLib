(*
 * Copyright 2022 Mirko Bianco (email: writetomirko@gmail.com)
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

unit Fido.Functional.Tries;

interface

uses
  System.SysUtils,
  System.Threading,

  Spring,

  Fido.Functional,
  Fido.Utilities;

type
  TExceptionClass = class of Exception;

  TryOut<T> = record
  private
    FFunctorFunc: Func<T>;
    FMonadFunc: Func<Context<T>>;

    function Resolve: T;
  public
    constructor New(const FunctorFunc: Func<T>); overload;
    constructor New(const MonadFunc: Func<Context<T>>); overload;
    constructor New(const Context: Context<T>); overload;

    function Match(const OnFailure: OnFailureEvent<T>; const OnFinally: TProc = nil): Context<T>; overload;
    function Match(const ExceptionClass: TExceptionClass; const ErrorMessage: string = ''; const OnFinally: TProc = nil): Context<T>; overload;
    function Match(const OnFinally: TProc): Context<T>; overload;
  end;

  &Try<T> = record
  private
    FValue: Context<T>;

  public
    constructor New(const Value: T); overload;
    constructor New(const Value: Context<T>); overload;

    class operator Implicit(const Value: &Try<T>): Context<T>;
    class operator Implicit(const Value: Context<T>): &Try<T>;

    function Map<TOut>(const Func: Context<T>.FunctorFunc<TOut>): TryOut<TOut>; overload; //Functor and Applicative
    function Map<TOut>(const Func: Context<T>.MonadFunc<TOut>): TryOut<TOut>; overload; //Monad

    function MapAsync<TOut>(const Func: Context<T>.FunctorFunc<TOut>; const Timeout: Cardinal = INFINITE; const Paused: Boolean = False): TryOut<TOut>; overload; //Functor and Applicative
    function MapAsync<TOut>(const Func: Context<T>.MonadFunc<TOut>; const Timeout: Cardinal = INFINITE; const Paused: Boolean = False): TryOut<TOut>; overload; //Monad

    function Match(const OnFailure: OnFailureEvent<T>; const OnFinally: TProc = nil): Context<T>; overload;
    function Match(const ExceptionClass: TExceptionClass; const ErrorMessage: string = ''; const OnFinally: TProc = nil): Context<T>; overload;
    function Match(const OnFinally: TProc): Context<T>; overload;

    function Match: Context<Boolean>; overload;
  end;

implementation

{$REGION ' &Try<T> '}
class operator &Try<T>.Implicit(const Value: &Try<T>): Context<T>;
begin
  Result := Value.FValue;
end;

class operator &Try<T>.Implicit(const Value: Context<T>): &Try<T>;
begin
  Result := &Try<T>.New(Value);
end;

function &Try<T>.Map<TOut>(const Func: Context<T>.MonadFunc<TOut>): TryOut<TOut>;
var
  LValue: Context<T>;
begin
  LValue := FValue;
  Result := TryOut<TOut>.New(function: Context<TOut>
    begin
      Result := LValue.Map<TOut>(Func);
    end);
end;

function &Try<T>.Map<TOut>(const Func: Context<T>.FunctorFunc<TOut>): TryOut<TOut>;
var
  LValue: Context<T>;
begin
  LValue := FValue;
  Result := TryOut<TOut>.New(function: TOut
    begin
      Result :=  LValue.Map<TOut>(Func);
    end);
end;

function &Try<T>.MapAsync<TOut>(
  const Func: Context<T>.MonadFunc<TOut>;
  const Timeout: Cardinal;
  const Paused: Boolean): TryOut<TOut>;
var
  LValue: Context<TOut>;
begin
  LValue := Context<T>.New(FValue).MapAsync<TOut>(Func, Timeout, Paused);

  Result := TryOut<TOut>.New(function: Context<TOut>
    begin
      Result := LValue;
    end);
end;

function &Try<T>.Match: Context<Boolean>;
var
  LCalculatedValue: T;
begin
  try
    LCalculatedValue := FValue;
    Result := True;
  except
    on E: Exception do
      Result :=  False;
  end;
end;

function &Try<T>.Match(const OnFinally: TProc): Context<T>;
var
  LOnFinally: TProc;
  LCalculatedValue: T;
begin
  LOnFinally := OnFinally;

  if not Assigned(LOnFinally) then
    LOnFinally := procedure
      begin
      end;

  try
    LCalculatedValue := FValue;
    Result := LCalculatedValue;
  finally
    LOnFinally();
  end;
end;

function &Try<T>.Match(
  const ExceptionClass: TExceptionClass;
  const ErrorMessage: string;
  const OnFinally: TProc): Context<T>;
var
  LOnFinally: TProc;
  LCalculatedValue: T;
begin
  LOnFinally := OnFinally;

  if not Assigned(LOnFinally) then
    LOnFinally := procedure
      begin
      end;

  try
    try
      LCalculatedValue := FValue;
      Result := LCalculatedValue;
    except
      on E: Exception do
        raise ExceptionClass.Create(Utilities.IfThen(ErrorMessage.IsEmpty, E.Message, Format(ErrorMessage, [E.Message])))
    end;
  finally
    LOnFinally();
  end;
end;

function &Try<T>.MapAsync<TOut>(
  const Func: Context<T>.FunctorFunc<TOut>;
  const Timeout: Cardinal;
  const Paused: Boolean): TryOut<TOut>;
var
  LValue: Context<TOut>;
begin
  LValue := Context<T>.New(FValue).MapAsync<TOut>(Func, Timeout, Paused);
  Result := TryOut<TOut>.New(function: Context<TOut>
    begin
      Result := LValue;
    end);
end;

function &Try<T>.Match(
  const OnFailure: OnFailureEvent<T>;
  const OnFinally: TProc): Context<T>;
var
  LOnFailure: OnFailureEvent<T>;
  LOnFinally: TProc;
  LCalculatedValue: T;
begin
  LOnFailure := OnFailure;
  LOnFinally := OnFinally;

  if not Assigned(LOnFailure) then
    LOnFailure := function(const Exc: TObject): T
      begin
        raise Exc;
      end;
  if not Assigned(LOnFinally) then
    LOnFinally := procedure
      begin
      end;

  try
    try
      LCalculatedValue := FValue;
      Result := LCalculatedValue;
    except
      on E: Exception do
        Result :=  LOnFailure(E);
    end;
  finally
    LOnFinally();
  end;
end;

constructor &Try<T>.New(const Value: T);
begin
  FValue := Value;
end;

constructor &Try<T>.New(const Value: Context<T>);
begin
  FValue := Value;
end;
{$ENDREGION}

{$REGION ' TryOut<T> '}
function TryOut<T>.Match(
  const OnFailure: OnFailureEvent<T>;
  const OnFinally: TProc): Context<T>;
var
  LOnFailure: OnFailureEvent<T>;
  LOnFinally: TProc;
begin
  LOnFailure := OnFailure;
  LOnFinally := OnFinally;

  if not Assigned(LOnFailure) then
    LOnFailure := function(const Exc: TObject): T
      begin
        raise Exc;
      end;
  if not Assigned(LOnFinally) then
    LOnFinally := procedure
      begin
      end;

  try
    try
      Result := Resolve;
    except
      on E: Exception do
        Result := LOnFailure(E);
    end;
  finally
    LOnFinally();
  end;
end;

function TryOut<T>.Match(const OnFinally: TProc): Context<T>;
var
  LOnFinally: TProc;
begin
  LOnFinally := OnFinally;

  if not Assigned(LOnFinally) then
    LOnFinally := procedure
      begin
      end;

  try
    Result := Resolve;
  finally
    LOnFinally();
  end;
end;

function TryOut<T>.Match(
  const ExceptionClass: TExceptionClass;
  const ErrorMessage: string;
  const OnFinally: TProc): Context<T>;
var
  LOnFinally: TProc;
begin
  LOnFinally := OnFinally;

  if not Assigned(LOnFinally) then
    LOnFinally := procedure
      begin
      end;

  try
    try
      Result := Resolve;
    except
      on E: Exception do
        raise ExceptionClass.Create(Utilities.IfThen(ErrorMessage.IsEmpty, E.Message, Format(ErrorMessage, [E.Message])))
    end;
  finally
    LOnFinally();
  end;
end;

constructor TryOut<T>.New(const MonadFunc: Func<Context<T>>);
begin
  FFunctorFunc := nil;
  FMonadFunc := MonadFunc;
end;

function TryOut<T>.Resolve: T;
begin
  if Assigned(FFunctorFunc) then
    Result := FFunctorFunc()
  else
    Result := FMonadFunc();
end;

constructor TryOut<T>.New(const FunctorFunc: Func<T>);
begin
  FFunctorFunc := FunctorFunc;
  FMonadFunc := nil;
end;

constructor TryOut<T>.New(const Context: Context<T>);
begin
  FFunctorFunc := Context;
  FMonadFunc := nil;
end;
{$ENDREGION}

end.
