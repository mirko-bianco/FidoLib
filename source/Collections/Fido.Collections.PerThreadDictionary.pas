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
  {$else}
  Posix.Pthread,
  {$endif}
  System.SyncObjs,
  System.Threading,
  System.SysUtils,
  System.Classes,
  System.IOUtils,

  Spring,
  Spring.Collections,

  Fido.Utilities,
  Fido.Collections.PerXDictionary.Intf;

type
  // This dictionary store one item per thread.
  TPerThreadDictionary<T> = class(TInterfacedObject, IPerXDictionary<T>)
  type
    TThreadId = Int64;
  strict private
    FLock: TLightweightMREW;
    FFactoryFunc: TFunc<T>;
  protected
    FItems: IDictionary<TThreadId, T>;
  public
    constructor Create(const Ownership: TDictionaryOwnerships; const FactoryFunc: TFunc<T>); reintroduce;
    destructor Destroy; override;

    function GetCurrent: T;
    procedure ReleaseCurrent;

    function GetItems: IEnumerable<T>;
  end;

implementation

{ TPerThreadDictionary<T> }

constructor TPerThreadDictionary<T>.Create(
  const Ownership: TDictionaryOwnerships;
  const FactoryFunc: TFunc<T>);
begin
  inherited Create;
  FItems := Spring.Collections.TCollections.CreateDictionary<TThreadId, T>(Ownership);
  FFactoryFunc := Utilities.CheckNotNullAndSet<TFunc<T>>(FactoryFunc, 'FFactoryFunc');
end;

destructor TPerThreadDictionary<T>.Destroy;
begin
  inherited;
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

function TPerThreadDictionary<T>.GetItems: IEnumerable<T>;
begin
  Result := FItems.Values;
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
