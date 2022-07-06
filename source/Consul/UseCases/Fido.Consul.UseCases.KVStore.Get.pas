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

unit Fido.Consul.UseCases.KVStore.Get;

interface

uses
  System.NetEncoding,

  Spring,
  Spring.Collections,

  Fido.Utilities,
  Fido.Functional,
  Fido.Functional.Retries,
  Fido.DesignPatterns.Retries,
  Fido.Api.Client.Consul.KVStore.V1.Intf,
  Fido.Consul.Gateways.KVStore.Intf,
  Fido.Consul.UseCases.KVStore.Get.Intf;

type
  TConsulKVStoreGetKeyUseCase = class(TInterfacedObject, IConsulKVStoreGetKeyUseCase)
  private
    FGateway: IConsulKVStoreApiGateway;

    function GetFirstValue(const List: IReadonlyList<IKVStoreGetResponseItem>): string;
    function Decode(const Value: string): string;
    function DoGet(const Timeout: Cardinal): Context<string>.MonadFunc<IReadonlyList<IKVStoreGetResponseItem>>;
  public
    constructor Create(const Gateway: IConsulKVStoreApiGateway);

    function Run(const Key: string; const Timeout: Cardinal = INFINITE): Context<string>;
  end;

implementation

{ TConsulKVStoreGetKeyUseCase }

constructor TConsulKVStoreGetKeyUseCase.Create(const Gateway: IConsulKVStoreApiGateway);
begin
  inherited Create;

  FGateway := Utilities.CheckNotNullAndSet(Gateway, 'Api');
end;

function TConsulKVStoreGetKeyUseCase.Decode(const Value: string): string;
begin
  Result := TNetEncoding.Base64.Decode(Value);
end;

function TConsulKVStoreGetKeyUseCase.GetFirstValue(const List: IReadonlyList<IKVStoreGetResponseItem>): string;
begin
  Result := '';
  if (Assigned(List) and (List.Count = 1)) then
    Result := List[0].Value;
end;

function TConsulKVStoreGetKeyUseCase.DoGet(const Timeout: Cardinal): Context<string>.MonadFunc<IReadonlyList<IKVStoreGetResponseItem>>;
var
  Gateway: IConsulKVStoreApiGateway;
begin
  Gateway := FGateway;
  Result := function(const Key: string): Context<IReadonlyList<IKVStoreGetResponseItem>>
    begin
      Result := Gateway.Get(Key, Timeout);
    end;
end;

function TConsulKVStoreGetKeyUseCase.Run(
  const Key: string;
  const Timeout: Cardinal): Context<string>;
begin
  Result := Retry<string>.New(Key).Map<IReadonlyList<IKVStoreGetResponseItem>>(DoGet(Timeout)).Map<string>(GetFirstValue).Map<string>(Decode);
end;

end.

