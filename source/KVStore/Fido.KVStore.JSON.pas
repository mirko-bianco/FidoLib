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

unit Fido.KVStore.JSON;

interface

uses
  System.SysUtils,

  Spring,

  Fido.Functional,
  Fido.KVStore.Intf,
  Fido.JSON.Marshalling;

type
  JSONKVStore = record
  public
    class function Get<T>(const KVStore: IKVStore; const Key: string; const Timeout: Cardinal = INFINITE): T; static;
    class function Put<T>(const KVStore: IKVStore; const Key: string; const Value: T; const Timeout: Cardinal = INFINITE): Boolean; static;
  end;

implementation

{ JSONKVStore }

class function JSONKVStore.Get<T>(
  const KVStore: IKVStore;
  const Key: string;
  const Timeout: Cardinal): T;
var
  Value: Context<string>;
begin
  Guard.CheckNotNull(KVStore, 'KVStore');

  Value := KVStore.Get(Key, Timeout);
  if Value.Value.IsEmpty then
    Exit(Default(T));

  Result := JSONUnmarshaller.To<T>(Value);
end;

class function JSONKVStore.Put<T>(
  const KVStore: IKVStore;
  const Key: string;
  const Value: T;
  const Timeout: Cardinal): Boolean;
var
  CallResult: Context<Boolean>;
begin
  Guard.CheckNotNull(KVStore, 'KVStore');

  CallResult := KVStore.Put(Key, JSONMarshaller.From<T>(Value), Timeout);
  Result := CallResult;
end;

end.
