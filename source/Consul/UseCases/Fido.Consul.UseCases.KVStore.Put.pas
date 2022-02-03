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

  Spring,
  Spring.Collections,

  Fido.DesignPatterns.Retries,
  Fido.Api.Client.Consul.KVStore.V1.Intf,
  Fido.Consul.UseCases.KVStore.Put.Intf;

type
  TConsulKVStorePutKeyUseCase = class(TInterfacedObject, IConsulKVStorePutKeyUseCase)
  private
    FApi: IConsulKVStoreApiV1;
  public
    constructor Create(const Api: IConsulKVStoreApiV1);

    function Run(const Key: string; const Value: string): Boolean;
  end;

implementation

{ TConsulKVStorePutKeyUseCase }

constructor TConsulKVStorePutKeyUseCase.Create(const Api: IConsulKVStoreApiV1);
begin
  inherited Create;

  Guard.CheckNotNull(Api, 'Api');
  FApi := Api;
end;

function TConsulKVStorePutKeyUseCase.Run(
  const Key: string;
  const Value: string): Boolean;
begin
  Result := Retries.Run<Boolean>(
    function: Boolean
    begin
      Result := FApi.Put(Key, Value);
    end,
    Retries.GetRetriesOnExceptionFunc());
end;

end.

