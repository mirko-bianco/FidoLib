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

unit Fido.Api.Client.Consul.KVStore.V1.Intf;

interface

uses
  System.SysUtils,
  Rest.Types,

  Spring.Collections,

  Fido.Api.Client.VirtualApi.Attributes,
  Fido.Api.Client.VirtualApi.Intf,

  Fido.Consul.Types,
  Fido.Api.Client.Consul.Constants;

type
  IKVStoreGetResponseItem = interface(IInvokable)
    ['{8BCA6FBB-6AD8-46D3-BC29-0A30D0D2E793}']

    function CreateIndex: Integer;
    function ModifyIndex: Integer;
    function LockIndex: Integer;
    function Key: string;
    function Flags: integer;
    function Value: string;
    function Session: string;
  end;

  IConsulKVStoreApiV1 = interface(IClientVirtualApi)
    ['{D81640A7-D44C-4F23-B924-40E04D019F83}']

    [Endpoint(rmGet, '/v1/kv/{key}')]
    [HeaderParam('Token', CONSUL_TOKEN)]
    function Get(const Key: string): IReadonlyList<IKVStoreGetResponseItem>;

    [Endpoint(rmPut, '/v1/kv/{key}')]
    [HeaderParam('Token', CONSUL_TOKEN)]
    [RawRequestParam('Value')]
    function Put(const Key: string; const Value: string): Boolean;

    [Endpoint(rmDelete, '/v1/kv/{key}')]
    [HeaderParam('Token', CONSUL_TOKEN)]
    function Delete(const Key: string): Boolean;
  end;

implementation

end.

