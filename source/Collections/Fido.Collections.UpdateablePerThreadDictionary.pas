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

unit Fido.Collections.UpdateablePerThreadDictionary;

interface

uses
  System.SysUtils,

  Spring,
  Spring.Collections,

  Fido.Collections.UpdateablePerXDictionary.Intf,
  Fido.Collections.PerThreadDictionary;

type
  TUpdateablePerThreadDictionary<T, TUpdateable> = class(TPerThreadDictionary<T>, IUpdatablePerXDictionary<T, TUpdateable>)
  strict private
    FPredicate: TProc<T, TUpdateable>;
    FUpdateableValue: TUpdateable;

    procedure SetUpdateableValue(const Updateable: TUpdateable);
    function GetUpdateableValue: TUpdateable;
  public
    constructor Create(const Ownership: TDictionaryOwnerships; const FactoryFunc: TFunc<T>; const Predicate: TProc<T, TUpdateable>); reintroduce;

    property UpdateableValue: TUpdateable read GetUpdateableValue write SetUpdateableValue;
  end;

implementation

{ TUpdateablePerThreadDictionary<T, TUpdateable> }

constructor TUpdateablePerThreadDictionary<T, TUpdateable>.Create(
  const Ownership: TDictionaryOwnerships;
  const FactoryFunc: TFunc<T>;
  const Predicate: TProc<T, TUpdateable>);
begin
  Guard.CheckTrue(Assigned(Predicate), 'Predicate');
  inherited Create(Ownership, FactoryFunc);
  FPredicate := Predicate;
end;

function TUpdateablePerThreadDictionary<T, TUpdateable>.GetUpdateableValue: TUpdateable;
begin
  result := FUpdateableValue;
end;

procedure TUpdateablePerThreadDictionary<T, TUpdateable>.SetUpdateableValue(const Updateable: TUpdateable);
begin
  FUpdateableValue := Updateable;

  with FItems.GetEnumerator do
    while MoveNext do
      FPredicate(GetCurrent.Value, Updateable);
end;

end.
