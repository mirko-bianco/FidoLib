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

unit Fido.Consul.UseCases.KVStore.Delete;

interface

uses
  System.NetEncoding,

  Spring,
  Spring.Collections,

  Fido.Utilities,
  Fido.Functional,
  Fido.Functional.Retries,
  Fido.DesignPatterns.Retries,
  Fido.Consul.Gateways.KVStore.Intf,
  Fido.Consul.UseCases.KVStore.Delete.Intf;

type
  TConsulKVStoreDeleteKeyUseCase = class(TInterfacedObject, IConsulKVStoreDeleteKeyUseCase)
  private
    FGateway: IConsulKVStoreApiGateway;

    function DoDelete(const Timeout: Cardinal): Context<string>.MonadFunc<Boolean>;
  public
    constructor Create(const Gateway: IConsulKVStoreApiGateway);

    function Run(const Key: string; const Timeout: Cardinal = INFINITE): Context<Boolean>;
  end;

implementation

{ TConsulKVStoreDeleteKeyUseCase }

constructor TConsulKVStoreDeleteKeyUseCase.Create(const Gateway: IConsulKVStoreApiGateway);
begin
  inherited Create;

  FGateway := Utilities.CheckNotNullAndSet(Gateway, 'Gateway');
end;

function TConsulKVStoreDeleteKeyUseCase.DoDelete(const Timeout: Cardinal): Context<string>.MonadFunc<Boolean>;
var
  Gateway: IConsulKVStoreApiGateway;
begin
  Gateway := FGateway;

  Result := function(const Key: string): Context<Boolean>
    begin
      Result := Gateway.Delete(Key, Timeout);
    end;
end;

function TConsulKVStoreDeleteKeyUseCase.Run(
  const Key: string;
  const Timeout: Cardinal): Context<Boolean>;
begin
  Result := Retry<string>.New(Key).Map<Boolean>(DoDelete(Timeout), Retries.GetRetriesOnExceptionFunc());
end;

end.

