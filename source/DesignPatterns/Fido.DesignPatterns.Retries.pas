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

unit Fido.DesignPatterns.Retries;

interface

uses
  System.SysUtils,

  Spring,
  Spring.Collections,

  Fido.Api.Client.Exception;

type
  Retries = record
    class function GetRetriesOnExceptionFunc: Predicate<Exception>; static;

    class function Run<T>(const Func: Func<T>; const RetryOnException: Predicate<Exception> = nil; const MaxRetries: Integer = 3; const RetryIntervalInMSec: Integer = 250): T; overload; static;
    class function Run(const Proc: Action; const RetryOnException: Predicate<Exception> = nil; const MaxRetries: Integer = 3; const RetryIntervalInMSec: Integer = 250): Boolean; overload; static;
  end;

implementation

{ Retries }

class function Retries.GetRetriesOnExceptionFunc: Predicate<Exception>;
begin
  Result := function(const Exc: Exception): Boolean
  begin
    Result := Exc.InheritsFrom(EFidoClientApiException) and
      TCollections.CreateList<Integer>([503, 504]).Contains(EFidoClientApiException(Exc).ErrorCode);
  end;
end;

class function Retries.Run(
  const Proc: Action;
  const RetryOnException: Predicate<Exception>;
  const MaxRetries: Integer;
  const RetryIntervalInMSec: Integer): Boolean;
var
  OnException: Predicate<Exception>;
  Index: Integer;
  FailCount: Integer;
begin
  OnException :=
    function(const Exc: Exception): Boolean
    begin
      Result := True;
    end;

  if Assigned(RetryOnException) then
    OnException := RetryOnException;

  Result := False;
  FailCount := 0;
  for Index := 1 to MaxRetries do
  begin
    try
      Proc();
      Result := True;
      Exit;
    except
      on E: Exception do
      begin
        if not OnException(E) then
          raise;

        Inc(FailCount);
        if FailCount >= MaxRetries then
          raise
        else
          Sleep(RetryIntervalInMSec);
      end;
    end;
  end;
end;

class function Retries.Run<T>(
  const Func: Func<T>;
  const RetryOnException: Predicate<Exception>;
  const MaxRetries: Integer;
  const RetryIntervalInMSec: Integer): T;
var
  OnException: Predicate<Exception>;
  Index: Integer;
  FailCount: Integer;
  Value: TClass;
begin
  OnException :=
    function(const Exc: Exception): Boolean
    begin
      Result := True;
    end;

  if Assigned(RetryOnException) then
    OnException := RetryOnException;

  FailCount := 0;
  for Index := 1 to MaxRetries do
  begin
    try
      Result := Func();
      Exit;
    except
      on E: Exception do
      begin
        if not OnException(E) then
          raise;

        Inc(FailCount);
        if FailCount >= MaxRetries then
          raise
        else
          Sleep(RetryIntervalInMSec);
      end;
    end;
  end;
end;

end.
