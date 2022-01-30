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

  Fido.DesignPatterns.Retries,
  Fido.Api.Client.Consul.KVStore.V1.Intf,
  Fido.Consul.UseCases.KVStore.Get.Intf;

type
  TConsulKVStoreGetKeyUseCase = class(TInterfacedObject, IConsulKVStoreGetKeyUseCase)
  private
    FApi: IConsulKVStoreApiV1;
  public
    constructor Create(const Api: IConsulKVStoreApiV1);

    function Run(const Key: string): string;
  end;

implementation

{ TConsulKVStoreGetKeyUseCase }

constructor TConsulKVStoreGetKeyUseCase.Create(const Api: IConsulKVStoreApiV1);
begin
  inherited Create;

  Guard.CheckNotNull(Api, 'Api');
  FApi := Api;
end;

function TConsulKVStoreGetKeyUseCase.Run(const Key: string): string;
var
  ApiResponse: IReadonlyList<IKVStoreGetResponseItem>;
begin
  Result := '';

  ApiResponse := Retries.Run<IReadonlyList<IKVStoreGetResponseItem>>(
    function: IReadonlyList<IKVStoreGetResponseItem>
    begin
      Result := FApi.Get(Key);
    end,
    Retries.GetRetriesOnExceptionFunc());

  if not Assigned(ApiResponse) then
    Exit;

  if ApiResponse.Count <> 1 then
    Exit;

   result := TNetEncoding.Base64.Decode(ApiResponse[0].Value);
end;

end.

