(*
 * Copyright 2022 Mirko Bianco (email: writetomirko@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without Apiriction, including without limitation the rights
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

unit Fido.Consul.Gateways.KVStore;

interface

uses
  Generics.Collections,

  Spring,
  Spring.Collections,

  Fido.Utilities,
  Fido.Functional,
  Fido.Api.Client.Consul.KVStore.V1.Intf,
  Fido.Consul.Gateways.KVStore.Intf;

type
  TConsulKVStoreApiGateway = class(TInterfacedObject, IConsulKVStoreApiGateway)
  private
    FConsulKVStoreApiV1: IConsulKVStoreApiV1;

    function DoGet(const Key: string): IReadonlyList<IKVStoreGetResponseItem>;
    function DoDelete(const Key: string): Boolean;
    function DoPut(const Params: TArray<string>): Boolean;
  public
    constructor Create(const ConsulKVStoreApiV1: IConsulKVStoreApiV1);

    function Get(const Key: string; const Timeout: Cardinal = INFINITE): Context<IReadonlyList<IKVStoreGetResponseItem>>;

    function Put(const Key: string; const Value: string; const Timeout: Cardinal = INFINITE): Context<Boolean>;

    function Delete(const Key: string; const Timeout: Cardinal = INFINITE): Context<Boolean>;
  end;

implementation

{ TConsulKVStoreApiGateway }

constructor TConsulKVStoreApiGateway.Create(const ConsulKVStoreApiV1: IConsulKVStoreApiV1);
begin
  inherited Create;

  FConsulKVStoreApiV1 := Utilities.CheckNotNullAndSet<IConsulKVStoreApiV1>(ConsulKVStoreApiV1, 'ConsulKVStoreApiV1');
end;

function TConsulKVStoreApiGateway.DoDelete(const Key: string): Boolean;
begin
  Result := FConsulKVStoreApiV1.Delete(Key);
end;

function TConsulKVStoreApiGateway.Delete(
  const Key: string;
  const Timeout: Cardinal = INFINITE): Context<Boolean>;
begin
  Result := Context<string>.New(Key).MapAsync<Boolean>(DoDelete, Timeout);
end;

function TConsulKVStoreApiGateway.DoGet(const Key: string): IReadonlyList<IKVStoreGetResponseItem>;
begin
  Result := FConsulKVStoreApiV1.Get(Key);
end;

function TConsulKVStoreApiGateway.Get(
  const Key: string;
  const Timeout: Cardinal = INFINITE): Context<IReadonlyList<IKVStoreGetResponseItem>>;
begin
  Result := Context<string>.New(Key).MapAsync<IReadonlyList<IKVStoreGetResponseItem>>(DoGet, Timeout);
end;

function TConsulKVStoreApiGateway.DoPut(const Params: TArray<string>): Boolean;
begin
  Result := FConsulKVStoreApiV1.Put(Params[0], Params[1]);
end;

function TConsulKVStoreApiGateway.Put(
  const Key: string;
  const Value: string;
  const Timeout: Cardinal = INFINITE): Context<Boolean>;
begin
  Result := Context<TArray<string>>.New([Key, Value]).MapAsync<Boolean>(DoPut, Timeout);
end;

end.

