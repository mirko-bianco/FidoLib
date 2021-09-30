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

{$I Jedi.inc}
unit Fido.Immutable;

interface

uses
  System.SysUtils,
  System.StrUtils,

  Spring,

  Fido.Exceptions;

type
  EImmutableException = class(EFidoException);

  {$IFDEF DELPHIX_SYDNEY_UP}
  TImmutable<T> = record
  private
    FValue: T;
    FAssigned: string;
  public
    class operator Implicit(const Value: TImmutable<T>): T;
    class operator Implicit(const Value: T): TImmutable<T>;

    class operator Assign(var Dest: TImmutable<T>; const [ref] Src: TImmutable<T>);

    constructor Make(const Value: T);
  end;
  {$ENDIF}

implementation

{ TImmutable<T> }

{$IFDEF DELPHIX_SYDNEY_UP}
class operator TImmutable<T>.Assign(var Dest: TImmutable<T>; const [ref] Src: TImmutable<T>);
begin
  if not Dest.FAssigned.IsEmpty then
    raise EImmutableException.Create('Immutables cannot be reassigned.');
  Dest.FValue := Src.FValue;
  Dest.FAssigned := Src.FAssigned;
end;

class operator TImmutable<T>.Implicit(const Value: TImmutable<T>): T;
begin
  Result := Value.FValue;
end;

class operator TImmutable<T>.Implicit(const Value: T): TImmutable<T>;
begin
  Result := TImmutable<T>.Make(Value);
end;

constructor TImmutable<T>.Make(const Value: T);
begin
  FValue := Value;
  FAssigned := 'Y';
end;
{$ENDIF}

end.
