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

unit Fido.Functional.Retries;

interface

uses
  System.SysUtils,
  System.Threading,

  Spring,

  Fido.Boxes,
  Fido.DesignPatterns.Retries,

  Fido.Functional;

type
  Retry = record
    class function Map<TOut>(const Func: Context<TOut>; const RetryOnException: Predicate<Exception> = nil; const MaxRetries: Integer = 3; const RetryIntervalInMSec: Integer = 250): Context<TOut>; overload; static;//Functor and Applicative

    class function MapAsync<TOut>(const Func: Context<TOut>; const Timeout: Cardinal = INFINITE; const Paused: Boolean = False; const RetryOnException: Predicate<Exception> = nil; const MaxRetries: Integer = 3; const RetryIntervalInMSec: Integer = 250): Context<TOut>; overload; static;//Functor and Applicative
  end;

  Retry<T> = record
  private
    FValue: Context<T>;

    {$HINTS OFF}
    function DoTry<TOut>(const Func: Context<T>.MonadFunc<TOut>; const Value: Context<T>): Func<TOut>; overload;
    function DoTry<TOut>(const Func: Context<T>.FunctorFunc<TOut>; const Value: Context<T>): Func<TOut>; overload;
    {$HINTS ON}
  public
    constructor New(const Value: T); overload;
    constructor New(const Value: Context<T>); overload;
    constructor New(const Func: Func<T>); overload;

    class operator Implicit(const Value: Retry<T>): Context<T>;
    class operator Implicit(const Value: Context<T>): Retry<T>;

    function Map<TOut>(const Func: Context<T>.FunctorFunc<TOut>; const RetryOnException: Predicate<Exception> = nil; const MaxRetries: Integer = 3; const RetryIntervalInMSec: Integer = 250): Context<TOut>; overload;//Functor and Applicative
    function Map<TOut>(const Func: Context<T>.MonadFunc<TOut>; const RetryOnException: Predicate<Exception> = nil; const MaxRetries: Integer = 3; const RetryIntervalInMSec: Integer = 250): Context<TOut>; overload;//Monad

    function MapAsync<TOut>(const Func: Context<T>.FunctorFunc<TOut>; const Timeout: Cardinal = INFINITE; const Paused: Boolean = False; const RetryOnException: Predicate<Exception> = nil; const MaxRetries: Integer = 3; const RetryIntervalInMSec: Integer = 250): Context<TOut>; overload;//Functor and Applicative
    function MapAsync<TOut>(const Func: Context<T>.MonadFunc<TOut>; const Timeout: Cardinal = INFINITE; const Paused: Boolean = False; const RetryOnException: Predicate<Exception> = nil; const MaxRetries: Integer = 3; const RetryIntervalInMSec: Integer = 250): Context<TOut>; overload;//Monad
  end;

implementation

{$REGION ' Retry<T> '}
function Retry<T>.DoTry<TOut>(
  const Func: Context<T>.MonadFunc<TOut>;
  const Value: Context<T>): Func<TOut>;
begin
  Result := function: TOut
    begin
     Result := Func(Value);
    end;
end;

function Retry<T>.Map<TOut>(
  const Func: Context<T>.MonadFunc<TOut>;
  const RetryOnException: Predicate<Exception>;
  const MaxRetries: Integer;
  const RetryIntervalInMSec: Integer): Context<TOut>;
begin
  Result := Retries.Run<TOut>(DoTry<TOut>(Func, FValue), RetryOnException, MaxRetries, RetryIntervalInMSec);
end;

function Retry<T>.MapAsync<TOut>(
  const Func: Context<T>.MonadFunc<TOut>;
  const Timeout: Cardinal;
  const Paused: Boolean;
  const RetryOnException: Predicate<Exception>;
  const MaxRetries: Integer;
  const RetryIntervalInMSec: Integer): Context<TOut>;
var
  Value: Context<T>;
  LSelf: Retry<T>;
begin
  LSelf := Self;
  Value := FValue;
  Result := Context<TOut>.New(
    function: TOut
    begin
      Result := Retries.Run<TOut>(LSelf.DoTry<TOut>(Func, Value), RetryOnException, MaxRetries, RetryIntervalInMSec);
    end,
    Timeout,
    Paused);
end;

function Retry<T>.MapAsync<TOut>(
  const Func: Context<T>.FunctorFunc<TOut>;
  const Timeout: Cardinal;
  const Paused: Boolean;
  const RetryOnException: Predicate<Exception>;
  const MaxRetries: Integer;
  const RetryIntervalInMSec: Integer): Context<TOut>;
var
  Value: Context<T>;
  LSelf: Retry<T>;
begin
  LSelf := Self;
  Value := FValue;
  Result := Context<TOut>.New(
    function: TOut
    begin
      Result := Retries.Run<TOut>(LSelf.DoTry<TOut>(Func, Value), RetryOnException, MaxRetries, RetryIntervalInMSec);
    end,
    Timeout,
    Paused);
end;

function Retry<T>.DoTry<TOut>(
  const Func: Context<T>.FunctorFunc<TOut>;
  const Value: Context<T>): Func<TOut>;
begin
  Result := function: TOut
    begin
     Result := Func(Value);
    end;
end;

class operator Retry<T>.Implicit(const Value: Context<T>): Retry<T>;
begin
  Result := Retry<T>.New(Value);
end;

class operator Retry<T>.Implicit(const Value: Retry<T>): Context<T>;
begin
  Result := Value.FValue;
end;

function Retry<T>.Map<TOut>(
  const Func: Context<T>.FunctorFunc<TOut>;
  const RetryOnException: Predicate<Exception>;
  const MaxRetries: Integer;
  const RetryIntervalInMSec: Integer): Context<TOut>;
begin
  Result := Retries.Run<TOut>(DoTry<TOut>(Func, FValue), RetryOnException, MaxRetries, RetryIntervalInMSec);
end;

constructor Retry<T>.New(const Value: T);
begin
  FValue := function: T
    begin
      Result := Value;
    end;
end;

constructor Retry<T>.New(const Func: Func<T>);
begin
  FValue := Func;
end;

constructor Retry<T>.New(const Value: Context<T>);
begin
  FValue := Value;
end;
{$ENDREGION}

{$REGION ' Retry '}
class function Retry.Map<TOut>(
  const Func: Context<TOut>;
  const RetryOnException: Predicate<Exception>;
  const MaxRetries: Integer;
  const RetryIntervalInMSec: Integer): Context<TOut>;
begin
  Result := Context<TOut>.New(function: TOut
    begin
      Result := Retries.Run<TOut>(Func, RetryOnException, MaxRetries, RetryIntervalInMSec);
    end);
end;

class function Retry.MapAsync<TOut>(
  const Func: Context<TOut>;
  const Timeout: Cardinal;
  const Paused: Boolean;
  const RetryOnException: Predicate<Exception>;
  const MaxRetries: Integer;
  const RetryIntervalInMSec: Integer): Context<TOut>;
begin
  Result := Context<TOut>.New(
    function: TOut
    begin
      Result := Retries.Run<TOut>(Func, RetryOnException, MaxRetries, RetryIntervalInMSec);
    end,
    Timeout,
    Paused);
end;
{$ENDREGION}

end.
