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

unit Fido.Functional;

interface

uses
  System.SysUtils,
  System.Threading,
  System.Rtti,

  Spring,

  Fido.Exceptions,
  Fido.Boxes;

type
  // https://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
  //Functor: applies a function to a wrapped value
  //Applicative: applies wrapped function to a wrapped value
  //Monad: applies a function(which returns a wrapped value) to a wrapped value

  EFunctionalContext = class(EFidoException);

  OnFailureEvent<T> = reference to function(const E: Exception): Nullable<T>;
  OnSuccessEvent<T> = reference to function(const Value: T): T;

  Context<T> = record
  public type
    FunctorFunc<TOut> = reference to function(const Value: T): TOut;
    MonadFunc<TOut> = reference to function(const Value: T): Context<TOut>;
    FunctorProc = reference to procedure(const Value: T);
  private
    FAssigned: string;
    FFunc: Func<T>;

    FAsyncFunc: Func<T>;
    FTimeout: Cardinal;
    FFuture: IFuture<T>;

    procedure StartFuture;
  public
    constructor New(const Value: T); overload;
    constructor New(const Value: Context<T>); overload;
    constructor New(const Func: Func<T>; const Timeout: Cardinal; const Paused: Boolean = False); overload;
    constructor New(const Func: Func<T>); overload;

    function IsAssigned: Boolean;
    function IsAsync: Boolean;

    class operator Implicit(const Value: Context<T>): T;
    class operator Implicit(const Value: Context<T>): Func<T>;
    class operator Implicit(const Value: T): Context<T>;
    class operator Implicit(const Func: Func<T>): Context<T>;

    function Value: T;

    function Map<TOut>(const Func: FunctorFunc<TOut>): Context<TOut>; overload; //Functor and Applicative
    function Map<TOut>(const Func: MonadFunc<TOut>): Context<TOut>; overload; //Monad

    function MapAsync<TOut>(const Func: FunctorFunc<TOut>; const Timeout: Cardinal = INFINITE; const Paused: Boolean = False): Context<TOut>; overload; //Functor and Applicative
    function MapAsync<TOut>(const Func: MonadFunc<TOut>; const Timeout: Cardinal = INFINITE; const Paused: Boolean = False): Context<TOut>; overload; //Monad
  end;

  Void = record
  public
    class function MapProc<T>(const Func: Context<T>.FunctorFunc<Void>): Context<T>.FunctorProc; overload; static;
    class function MapProc<T>(const Proc: Context<T>.FunctorProc): Context<T>.FunctorFunc<Void>; overload; static;
    class function MapProc(const Proc: TProc): Context<Void>.FunctorFunc<Void>; overload; static;
    class function MapFunc<T>(const Func: Context<Void>.FunctorFunc<T>): TFunc<T>; overload; static;
    class function MapFunc<T>(const Func: TFunc<T>): Context<Void>.FunctorFunc<T>; overload; static;

    class function Map<T>(const Ctx: Context<T>): Context<Void>; static;

    class function Get: Void; static;
  end;

function CloneException(const Exc: Exception): Exception;

implementation

function CloneException(const Exc: Exception): Exception;
var
  Typ: TRttiType;
  Prop: TRttiProperty;
begin
  Result := Exc.ClassType.Create as Exception;

  Typ := TRttiContext.Create.GetType(Exc.ClassType);
  for Prop in Typ.GetProperties do
    if Prop.IsWritable then
      Prop.SetValue(Result, Prop.GetValue(Exc))
end;

{$REGION ' Context<T> '}
function Context<T>.Map<TOut>(const Func: MonadFunc<TOut>): Context<TOut>;
begin
  Result := Func(Self);
end;

function Context<T>.Map<TOut>(const Func: FunctorFunc<TOut>): Context<TOut>;
begin
  Result := Context<TOut>.New(Func(Self));
end;

function Context<T>.MapAsync<TOut>(
  const Func: MonadFunc<TOut>;
  const Timeout: Cardinal;
  const Paused: Boolean): Context<TOut>;
var
  LSelf: Context<T>;
begin
  LSelf := Self;
  Result := Context<TOut>.New(
    function: TOut
    begin
      Result := Func(LSelf);
    end,
    Timeout,
    Paused);
end;

function Context<T>.MapAsync<TOut>(
  const Func: FunctorFunc<TOut>;
  const Timeout: Cardinal;
  const Paused: Boolean): Context<TOut>;
var
  LSelf: Context<T>;
begin
  LSelf := Self;
  Result := Context<TOut>.New(
    function: TOut
    begin
      Result := Func(LSelf);
    end,
    Timeout,
    Paused);
end;

class operator Context<T>.Implicit(const Func: Func<T>): Context<T>;
begin
  Result := Context<T>.New(Func);
end;

class operator Context<T>.Implicit(const Value: Context<T>): Func<T>;
begin
  if Assigned(Value.FAsyncFunc) then
  begin
    if not Assigned(Value.FFuture) then
      Value.StartFuture;
    Result := function: T
      var
        InTime: Boolean;
      begin
        InTime := False;
        try
          InTime := Value.FFuture.Wait(Value.FTimeout);
        except
          on E: EAggregateException do
            with E.GetEnumerator do
            begin
              try
                while MoveNext do
                  raise CloneException(Current);
              finally
                Free;
              end;
            end;
        end;

        if InTime then
          Result := Value.FFuture.Value
        else
          raise EFunctionalContext.Create('Value could not be resolved within the timeout');
      end;
  end
  else if Assigned(Value.FFunc) then
    Result := Value.FFunc
  else
    raise EFunctionalContext.Create('Could not extract Value');
end;

class operator Context<T>.Implicit(const Value: Context<T>): T;
var
  Func: Func<T>;
begin
  Func := Value;
  Result := Func();
end;

function Context<T>.IsAssigned: Boolean;
begin
  Result := not FAssigned.IsEmpty;
end;

function Context<T>.IsAsync: Boolean;
begin
  Result := Assigned(FAsyncFunc);
end;

class operator Context<T>.Implicit(const Value: T): Context<T>;
begin
  Result := Context<T>.New(Value);
end;

constructor Context<T>.New(const Value: T);
begin
  FAssigned := 'True';
  FAsyncFunc := nil;
  FTimeout := 0;
  FFuture := nil;
  FFunc := function: T
    begin
      Result := Value;
    end;
end;

constructor Context<T>.New(const Func: Func<T>; const Timeout: Cardinal; const Paused: Boolean);
begin
  FAssigned := 'True';
  FAsyncFunc := Func;
  FTimeout := Timeout;
  FFuture := nil;
  if not Paused then
    FFuture := TTask.Future<T>(TFunc<T>(FAsyncFunc));
  FFunc := nil;
end;

constructor Context<T>.New(const Func: Func<T>);
begin
  FAssigned := 'True';
  FAsyncFunc := nil;
  FTimeout := 0;
  FFuture := nil;
  FFunc := Func;
end;

procedure Context<T>.StartFuture;
begin
  if Assigned(FFuture) then
    raise EFunctionalContext.Create('Future already started.');
  if not Assigned(FAsyncFunc) then
    raise EFunctionalContext.Create('Context is not async.');

  FFuture := TTask.Future<T>(TFunc<T>(FAsyncFunc));
end;

constructor Context<T>.New(const Value: Context<T>);
begin
  Self := Value;
end;

function Context<T>.Value: T;
var
  ResolvedValue: T;
begin
  ResolvedValue := Self;
  Result := ResolvedValue;
end;
{$ENDREGION}

{$REGION ' Void '}
class function Void.MapProc<T>(const Func: Context<T>.FunctorFunc<Void>): Context<T>.FunctorProc;
begin
  Result := procedure(const Value: T)
    begin
      Func(Value);
    end;
end;

class function Void.Get: Void;
var
  AVoid: Void;
begin
  Result := AVoid;
end;

class function Void.MapFunc<T>(const Func: Context<Void>.FunctorFunc<T>): TFunc<T>;
begin
  Result :=
    function: T
    begin
      Result := Func(Void.Get);
    end;
end;

class function Void.Map<T>(const Ctx: Context<T>): Context<Void>;
begin
  Result := function: Void
    begin
      Ctx.Value;
      Result := Void.Get;
    end;
end;

class function Void.MapFunc<T>(const Func: TFunc<T>): Context<Void>.FunctorFunc<T>;
begin
  Result := function(const AVoid: Void): T
    begin
      Result := Func();
    end;
end;

class function Void.MapProc(const Proc: TProc): Context<Void>.FunctorFunc<Void>;
begin
  Result := function(const Value: Void): Void
    begin
      Proc();
    end;
end;

class function Void.MapProc<T>(const Proc: Context<T>.FunctorProc): Context<T>.FunctorFunc<Void>;
begin
  Result := function(const Value: T): Void
    begin
      Proc(Value);
    end;
end;
{$ENDREGION}

end.
