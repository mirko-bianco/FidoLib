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

unit Fido.Functional.Ifs;

interface

uses
  System.SysUtils,
  System.Threading,

  Fido.Utilities,
  Fido.Functional;

type
  ThenElse = record
  private
    FFlagFunc: TFunc<Context<Boolean>>;

  public
    constructor New(const Flag: Context<Boolean>); overload;
    constructor New(const FlagFunc: TFunc<Context<Boolean>>); overload;

    class operator Implicit(const Value: ThenElse): Context<Boolean>;
    class operator Implicit(const Value: Context<Boolean>): ThenElse;

    function &Then<TOut>(const WhenTrue: Context<TOut>; const WhenFalse: Context<TOut>): Context<TOut>; overload;
    function &Then<TOut>(const WhenTrue: Context<TOut>): Context<TOut>; overload;
  end;

  FunctorThenElse<T> = record
  private
    FFlagFunc: Context<T>.FunctorFunc<Boolean>;
    FValue: Context<T>;

  public
    constructor New(const FlagFunc: Context<T>.FunctorFunc<Boolean>; const Value: Context<T>);

    function &Then<TOut>(const WhenTrue, WhenFalse: Context<T>.FunctorFunc<TOut>): Context<TOut>; overload;
    function &Then<TOut>(const WhenTrue: Context<T>.FunctorFunc<TOut>; const WhenFalse: Context<TOut>): Context<TOut>; overload;
    function &Then<TOut>(const WhenTrue: Context<T>.FunctorFunc<TOut>): Context<TOut>; overload;

    function &Then<TOut>(const WhenTrue: Context<TOut>; const WhenFalse: Context<TOut>): Context<TOut>; overload;
    function &Then<TOut>(const WhenTrue: Context<TOut>): Context<TOut>; overload;
  end;

  MonadThenElse<T> = record
  private
    FFlagFunc: Context<T>.MonadFunc<Boolean>;
    FFuncValue: TFunc<T>;

  public
    constructor New(const FlagFunc: Context<T>.MonadFunc<Boolean>; const FuncValue: TFunc<T>);

    function &Then<TOut>(const WhenTrue: Context<T>.FunctorFunc<TOut>; const WhenFalse: Context<TOut>): Context<TOut>; overload;
    function &Then<TOut>(const WhenTrue: Context<T>.FunctorFunc<TOut>): Context<TOut>; overload;

    function &Then<TOut>(const WhenTrue: Context<TOut>; const WhenFalse: Context<TOut>): Context<TOut>; overload;
    function &Then<TOut>(const WhenTrue: Context<TOut>): Context<TOut>; overload;
  end;

  &If<T> = record
  private
    FValue: Context<T>;

  public
    constructor New(const Value: T); overload;
    constructor New(const Value: Context<T>); overload;
    constructor New(const Func: TFunc<T>); overload;

    class operator Implicit(const Value: &If<T>): Context<T>;
    class operator Implicit(const Value: Context<T>): &If<T>;

    function Map(const Func: Context<T>.FunctorFunc<Boolean>): FunctorThenElse<T>; overload; //Functor and Applicative
    function Map(const Func: Context<T>.MonadFunc<Boolean>): MonadThenElse<T>; overload; //Monad
  end;

implementation

{$REGION ' &If<T> '}
class operator &If<T>.Implicit(const Value: &If<T>): Context<T>;
begin
  Result := Value.FValue;
end;

class operator &If<T>.Implicit(const Value: Context<T>): &If<T>;
begin
  Result := &If<T>.New(Value);
end;

function &If<T>.Map(const Func: Context<T>.MonadFunc<Boolean>): MonadThenElse<T>;
begin
  Result := MonadThenElse<T>.New(Func, Self.FValue);
end;

function &If<T>.Map(const Func: Context<T>.FunctorFunc<Boolean>): FunctorThenElse<T>;
begin
  Result := FunctorThenElse<T>.New(Func, Self.FValue);
end;

constructor &If<T>.New(const Value: T);
begin
  FValue := function: T
    begin
      Result := Value;
    end;
end;

constructor &If<T>.New(const Value: Context<T>);
begin
  FValue := function: T
    begin
      Result := Value;
    end;
end;

constructor &If<T>.New(const Func: TFunc<T>);
begin
  FValue := Func;
end;
{$ENDREGION}

{$REGION ' ThenElse '}
class operator ThenElse.Implicit(const Value: ThenElse): Context<Boolean>;
begin
  Result := Value.FFlagFunc();
end;

class operator ThenElse.Implicit(const Value: Context<Boolean>): ThenElse;
begin
  Result := ThenElse.New(Value);
end;

function ThenElse.&Then<TOut>(
  const WhenTrue: Context<TOut>;
  const WhenFalse: Context<TOut>): Context<TOut>;
var
  LSelf: ThenElse;
begin
  LSelf := Self;
  Result := function: TOut
  begin
    if LSelf.FFlagFunc() then
      Result := WhenTrue
    else
      Result := WhenFalse;
  end;
end;

constructor ThenElse.New(const Flag: Context<Boolean>);
var
  LFlag: Context<Boolean>;
begin
  LFlag := Flag;
  FFlagFunc := function: Context<Boolean>
    begin
      Result := LFlag;
    end;
end;

constructor ThenElse.New(const FlagFunc: TFunc<Context<Boolean>>);
begin
  FFlagFunc := FlagFunc;
end;

function ThenElse.&Then<TOut>(const WhenTrue: Context<TOut>): Context<TOut>;
var
  LSelf: ThenElse;
begin
  LSelf := Self;
  Result := function: TOut
  begin
    if LSelf.FFlagFunc() then
      Result := WhenTrue
    else
      Result := Context<TOut>.New(Default(TOut));
  end;
end;
{$ENDREGION}

{$REGION ' FunctorThenElse<T> '}
constructor FunctorThenElse<T>.New(const FlagFunc: Context<T>.FunctorFunc<Boolean>; const Value: Context<T>);
begin
  FFlagFunc := FlagFunc;
  FValue := Value;
end;

function FunctorThenElse<T>.&Then<TOut>(const WhenTrue: Context<TOut>): Context<TOut>;
begin
  Result := &Then<TOut>(WhenTrue, Context<TOut>.New(Default(TOut)));
end;

function FunctorThenElse<T>.&Then<TOut>(const WhenTrue, WhenFalse: Context<TOut>): Context<TOut>;
var
  LSelf: FunctorThenElse<T>;
begin
  LSelf := Self;
  Result := function: TOut
  begin
    if LSelf.FFlagFunc(LSelf.FValue) then
      Result := WhenTrue
    else
      Result := WhenFalse;
  end;
end;

function FunctorThenElse<T>.&Then<TOut>(const WhenTrue: Context<T>.FunctorFunc<TOut>): Context<TOut>;
begin
  Result := &Then<TOut>(WhenTrue, Context<TOut>.New(Default(TOut)));
end;

function FunctorThenElse<T>.&Then<TOut>(const WhenTrue: Context<T>.FunctorFunc<TOut>; const WhenFalse: Context<TOut>): Context<TOut>;
var
  LSelf: FunctorThenElse<T>;
begin
  LSelf := Self;
  Result := function: TOut
  begin
    if LSelf.FFlagFunc(LSelf.FValue) then
      Result := WhenTrue(LSelf.FValue)
    else
      Result := WhenFalse;
  end;
end;

function FunctorThenElse<T>.&Then<TOut>(const WhenTrue: Context<T>.FunctorFunc<TOut>; const WhenFalse: Context<T>.FunctorFunc<TOut>): Context<TOut>;
var
  LSelf: FunctorThenElse<T>;
begin
  LSelf := Self;
  Result := function: TOut
  begin
    if LSelf.FFlagFunc(LSelf.FValue) then
      Result := WhenTrue(LSelf.FValue)
    else
      Result := WhenFalse(LSelf.FValue);
  end;
end;
{$ENDREGION}

{$REGION ' MonadThenElse<T> '}
constructor MonadThenElse<T>.New(const FlagFunc: Context<T>.MonadFunc<Boolean>; const FuncValue: TFunc<T>);
begin
  FFlagFunc := FlagFunc;
  FFuncValue := FuncValue;
end;

function MonadThenElse<T>.&Then<TOut>(const WhenTrue: Context<T>.FunctorFunc<TOut>): Context<TOut>;
var
  LSelf: MonadThenElse<T>;
begin
  LSelf := Self;
  Result := function: TOut
    var
      Flag: Context<Boolean>;
    begin
      Flag := LSelf.FFlagFunc(LSelf.FFuncValue());
      if Flag then
        Result := WhenTrue(LSelf.FFuncValue())
      else
        Result := Context<TOut>.New(Default(TOut));
    end;
end;

function MonadThenElse<T>.&Then<TOut>(const WhenTrue: Context<T>.FunctorFunc<TOut>; const WhenFalse: Context<TOut>): Context<TOut>;
var
  LSelf: MonadThenElse<T>;
begin
  LSelf := Self;
  Result := function: TOut
    var
      Flag: Context<Boolean>;
    begin
      Flag := LSelf.FFlagFunc(LSelf.FFuncValue());
      if Flag then
        Result := WhenTrue(LSelf.FFuncValue())
      else
        Result := WhenFalse;
    end;
end;

function MonadThenElse<T>.&Then<TOut>(const WhenTrue: Context<TOut>): Context<TOut>;
var
  LSelf: MonadThenElse<T>;
begin
  LSelf := Self;
  Result := function: TOut
    var
      Flag: Context<Boolean>;
    begin
      Flag := LSelf.FFlagFunc(LSelf.FFuncValue());
      if Flag then
        Result := WhenTrue
      else
        Result := Context<TOut>.New(Default(TOut));
    end;
end;

function MonadThenElse<T>.&Then<TOut>(const WhenTrue, WhenFalse: Context<TOut>): Context<TOut>;
var
  LSelf: MonadThenElse<T>;
begin
  LSelf := Self;
  Result := function: TOut
    var
      Flag: Context<Boolean>;
    begin
      Flag := LSelf.FFlagFunc(LSelf.FFuncValue());
      if Flag then
        Result := WhenTrue
      else
        Result := WhenFalse;
    end;
end;
{$ENDREGION}

end.
