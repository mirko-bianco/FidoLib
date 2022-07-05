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

unit Fido.Consul.KVStore;

interface

uses
  Spring,

  Fido.Utilities,
  Fido.Functional,
  Fido.KVStore.Intf,
  Fido.Consul.UseCases.KVStore.Get.Intf,
  Fido.Consul.UseCases.KVStore.Put.Intf,
  Fido.Consul.UseCases.KVStore.Delete.Intf;

type
  TConsulKVStore = class(TInterfacedObject, IKVStore)
  private
    FGetUseCase: IConsulKVStoreGetKeyUseCase;
    FPutUseCase: IConsulKVStorePutKeyUseCase;
    FDeleteUseCase: IConsulKVStoreDeleteKeyUseCase;
  public
    constructor Create(const GetUseCase: IConsulKVStoreGetKeyUseCase; const PutUseCase: IConsulKVStorePutKeyUseCase; const DeleteUseCase: IConsulKVStoreDeleteKeyUseCase);

    function Get(const Key: string; const Timeout: Cardinal = INFINITE): Context<string>;
    function Put(const Key: string; const Value: string; const Timeout: Cardinal = INFINITE): Context<Boolean>;
    function Delete(const Key: string; const Timeout: Cardinal = INFINITE): Context<Boolean>;
  end;

implementation

{ TConsulKVStore }

constructor TConsulKVStore.Create(
  const GetUseCase: IConsulKVStoreGetKeyUseCase;
  const PutUseCase: IConsulKVStorePutKeyUseCase;
  const DeleteUseCase: IConsulKVStoreDeleteKeyUseCase);
begin
  inherited Create;

  FGetUseCase := Utilities.CheckNotNullAndSet(GetUseCase, 'GetUseCase');
  FPutUseCase := Utilities.CheckNotNullAndSet(PutUseCase, 'PutUseCase');
  FDeleteUseCase := Utilities.CheckNotNullAndSet(DeleteUseCase, 'DeleteUseCase');
end;

function TConsulKVStore.Delete(
  const Key: string;
  const Timeout: Cardinal): Context<Boolean>;
begin
  Result := FDeleteUseCase.Run(Key, Timeout);
end;

function TConsulKVStore.Get(
  const Key: string;
  const Timeout: Cardinal): Context<string>;
begin
  Result := FGetUseCase.Run(Key, Timeout);
end;

function TConsulKVStore.Put(
  const Key: string;
  const Value: string;
  const Timeout: Cardinal): Context<Boolean>;
begin
  Result := FPutUseCase.Run(Key, Value, Timeout);
end;

end.
