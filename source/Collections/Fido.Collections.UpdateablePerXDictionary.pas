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

unit Fido.Collections.UpdateablePerXDictionary;

interface

uses
  System.SysUtils,

  Spring,
  Spring.Collections,

  Fido.Utilities,
  Fido.Collections.PerXDictionary.Intf;

type
  TUpdateablePerXDictionary<T, TUpdateable> = class
  strict private
    FPredicate: Action<T, TUpdateable>;
    FUpdateableValue: TUpdateable;
    FDictionary: IPerXDictionary<T>;
  public
    constructor Create(const DictionaryFactoryFunc: Func<TDictionaryOwnerships, Func<T>, IPerXDictionary<T>>; const Ownership: TDictionaryOwnerships; const ValueFactoryFunc: Func<T>;
      const Predicate: Action<T, TUpdateable>); reintroduce;

    function GetCurrent: T;
    procedure ReleaseCurrent;

    procedure SetUpdateableValue(const Updateable: TUpdateable);
    function GetUpdateableValue: TUpdateable;
  end;

implementation

{ TUpdateablePerXDictionary<T, TUpdateable> }

constructor TUpdateablePerXDictionary<T, TUpdateable>.Create(
  const DictionaryFactoryFunc: Func<TDictionaryOwnerships, Func<T>, IPerXDictionary<T>>;
  const Ownership: TDictionaryOwnerships;
  const ValueFactoryFunc: Func<T>;
  const Predicate: Action<T, TUpdateable>);
begin
  inherited Create;
  FPredicate := Utilities.CheckNotNullAndSet<TProc<T, TUpdateable>>(Predicate, 'Predicate');
  FDictionary := Utilities.CheckNotNullAndSet<TFunc<TDictionaryOwnerships, TFunc<T>, IPerXDictionary<T>>>(DictionaryFactoryFunc, 'DictionaryFactoryFunc')(
    Ownership,
    Utilities.CheckNotNullAndSet<TFunc<T>>(ValueFactoryFunc, 'ValueFactoryFunc'));
end;

function TUpdateablePerXDictionary<T, TUpdateable>.GetCurrent: T;
begin
  Result := FDictionary.GetCurrent;
end;

function TUpdateablePerXDictionary<T, TUpdateable>.GetUpdateableValue: TUpdateable;
begin
  result := FUpdateableValue;
end;

procedure TUpdateablePerXDictionary<T, TUpdateable>.ReleaseCurrent;
begin
  FDictionary.ReleaseCurrent;
end;

procedure TUpdateablePerXDictionary<T, TUpdateable>.SetUpdateableValue(const Updateable: TUpdateable);
begin
  FUpdateableValue := Updateable;

  with FDictionary.GetItems.GetEnumerator do
    while MoveNext do
      FPredicate(GetCurrent, Updateable);
end;

end.
