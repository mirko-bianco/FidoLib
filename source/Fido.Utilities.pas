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

unit Fido.Utilities;

interface

uses
  System.SysUtils,

  Spring;

type
  TCheckPredicate = reference to function: Boolean;

  Utilities = record
    class function IfThen<T>(const PredicateFunc: TFunc<Boolean>; const IfTrue: T; const IfFalse: T): T; overload; static;
    class function IfThen<T>(const PredicateResult: Boolean; const IfTrue: T; const IfFalse: T): T; overload; static;

    class function TryStringToTGuid(const Input: string; out Guid: TGuid): Boolean; static;

    class function CheckNotNullAndSet<T>(const Value: T; const ArgumentName: String): T; static;
    class function CheckAndSet<T>(const Value: T; const Predicate: TCheckPredicate; const ErrorMessage: String): T; static;
  type
    F = record
      class function IsEmpty(const Value: string): TCheckPredicate; static;
      class function &Not(const Check: TCheckPredicate): TCheckPredicate; static;
      class function IsNotEmpty(const Value: string): TCheckPredicate; static;

    end;
  end;

implementation

class function Utilities.IfThen<T>(
  const PredicateFunc: TFunc<Boolean>;
  const IfTrue: T;
  const IfFalse: T): T;
begin
  Result := IfThen<T>(PredicateFunc(), IfTrue, IfFalse);
end;

class function Utilities.IfThen<T>(
  const PredicateResult: Boolean;
  const IfTrue: T;
  const IfFalse: T): T;
begin
  if PredicateResult then
    Result := IfTrue
  else
    Result := IfFalse;
end;

class function Utilities.TryStringToTGuid(
  const Input: string;
  out Guid: TGuid): Boolean;
begin
  Result := False;
  try
    Guid := StringToGuid(Input);
    Result := True;
  except
  end;
end;

class function Utilities.CheckAndSet<T>(
  const Value: T;
  const Predicate: TCheckPredicate;
  const ErrorMessage: String): T;
begin
  Spring.Guard.CheckTrue(Predicate, ErrorMessage);
  Result := Value;
end;

class function Utilities.CheckNotNullAndSet<T>(
  const Value: T;
  const ArgumentName: String): T;
begin
  Spring.Guard.CheckNotNull(Value, ArgumentName);
  Result := Value;
end;

class function Utilities.F.IsNotEmpty(const Value: string): TCheckPredicate;
begin
  Result := &Not(IsEmpty(Value));
end;

class function Utilities.F.&Not(const Check: TCheckPredicate): TCheckPredicate;
begin
  Result := function: Boolean
    begin
      Result := not Check;
    end;
end;

class function Utilities.F.IsEmpty(const Value: string): TCheckPredicate;
begin
  Result := function: Boolean
    begin
      Result := Value.IsEmpty;
    end;
end;

end.
