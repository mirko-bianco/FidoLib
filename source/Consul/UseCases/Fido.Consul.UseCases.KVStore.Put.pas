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

unit Fido.Consul.UseCases.KVStore.Put;

interface

uses
  System.NetEncoding,
  Generics.Collections,

  Spring,
  Spring.Collections,

  Fido.Utilities,
  Fido.Functional,
  Fido.Functional.Retries,
  Fido.DesignPatterns.Retries,
  Fido.Consul.Gateways.KVStore.Intf,
  Fido.Consul.UseCases.KVStore.Put.Intf;

type
  TConsulKVStorePutKeyUseCase = class(TInterfacedObject, IConsulKVStorePutKeyUseCase)
  private
    FGateway: IConsulKVStoreApiGateway;
    function DoPut(const Timeout: Cardinal): Context<TArray<string>>.MonadFunc<Boolean>;

  public
    constructor Create(const Gateway: IConsulKVStoreApiGateway);

    function Run(const Key: string; const Value: string; const Timeout: Cardinal = INFINITE): Context<Boolean>;
  end;

implementation

{ TConsulKVStorePutKeyUseCase }

constructor TConsulKVStorePutKeyUseCase.Create(const Gateway: IConsulKVStoreApiGateway);
begin
  inherited Create;

  FGateway := Utilities.CheckNotNullAndSet(Gateway, 'Api');
end;

function TConsulKVStorePutKeyUseCase.DoPut(const Timeout: Cardinal): Context<TArray<string>>.MonadFunc<Boolean>;
var
  Gateway: IConsulKVStoreApiGateway;
begin
  Gateway := FGateway;

  Result := function(const Params: TArray<string>): Context<Boolean>
    begin
      Result := Gateway.Put(Params[0], Params[1], Timeout);
    end;
end;

function TConsulKVStorePutKeyUseCase.Run(
  const Key: string;
  const Value: string;
  const Timeout: Cardinal): Context<Boolean>;
begin
  Result := Retry<TArray<string>>.New([Key, Value]).Map<Boolean>(DoPut(Timeout), Retries.GetRetriesOnExceptionFunc());
end;

end.

