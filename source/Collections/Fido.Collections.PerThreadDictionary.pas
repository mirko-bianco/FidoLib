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

unit Fido.Collections.PerThreadDictionary;

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  {$ifdef POSIX}
  Posix.Pthread,
  {$endif}
  System.SysUtils,

  Spring,
  Spring.Collections;

type
  // This dictionary store one item per thread.
  TPerThreadDictionary<T> = class
  type
    TThreadId = Integer;
  strict private
    FLock: IReadWriteSync;
    FFactoryFunc: TFunc<T>;
  protected
    FItems: IDictionary<TThreadId, T>;
  public
    constructor Create(const Ownership: TDictionaryOwnerships; const FactoryFunc: TFunc<T>); reintroduce;

    function GetCurrent: T;
    procedure ReleaseCurrent;
  end;

implementation

{ TPerThreadDictionary<T> }

constructor TPerThreadDictionary<T>.Create(
  const Ownership: TDictionaryOwnerships;
  const FactoryFunc: TFunc<T>);
begin
  Guard.CheckTrue(Assigned(FactoryFunc), 'FFactoryFunc');
  inherited Create;
  FLock := TMREWSync.Create;
  FItems := Spring.Collections.TCollections.CreateDictionary<Integer, T>(Ownership);
  FFactoryFunc := FactoryFunc;
end;

function TPerThreadDictionary<T>.GetCurrent: T;
var
  Item: T;
  Found: Boolean;
begin
  FLock.BeginRead;
  try
     Found := FItems.TryGetValue(GetCurrentThreadId, Item);
  finally
    FLock.EndRead;
  end;
  if not Found then
  begin
    FLock.BeginWrite;
    try
      Item := FFactoryFunc();
      FItems.Add(GetCurrentThreadId, Item);
    finally
      FLock.EndWrite;
    end;
  end;
  Result := Item;
end;

procedure TPerThreadDictionary<T>.ReleaseCurrent;
begin
  FLock.BeginWrite;
  try
    FItems.Remove(GetCurrentThreadId);
  finally
    FLock.EndWrite;
  end;
end;

end.
