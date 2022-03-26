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

unit Fido.Consul.DI.Registration;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IniFiles,

  Spring,
  Spring.Container,

  Fido.Api.Client.VirtualApi.json,
  Fido.Jwt.Manager.Intf,

  Fido.Containers,
  Fido.Api.Client.Consul.Constants,
  Fido.Api.Client.Consul.AgentService.V1.Intf,
  Fido.Api.Client.Consul.KVStore.V1.Intf,
  Fido.Api.Client.Consul.Configuration,
  Fido.KVStore.Intf,
  Fido.Consul.UseCases.Service.Register.Intf,
  Fido.Consul.UseCases.Service.Register,
  Fido.Consul.UseCases.Service.Deregister.Intf,
  Fido.Consul.UseCases.Service.Deregister,
  Fido.Consul.UseCases.KVStore.Get.Intf,
  Fido.Consul.UseCases.KVStore.Get,
  Fido.Consul.UseCases.KVStore.Put.Intf,
  Fido.Consul.UseCases.KVStore.Put,
  Fido.Consul.UseCases.KVStore.Delete.Intf,
  Fido.Consul.UseCases.KVStore.Delete,
  Fido.Consul.KVStore,
  Fido.Consul.Service.Intf,
  Fido.Consul.Service;

procedure Register(const Container: TContainer; const IniFile: TMemIniFile);

implementation

procedure Register(
  const Container: TContainer;
  const IniFile: TMemIniFile);
begin
  Container.RegisterType<IConsulClientVirtualApiConfiguration>.DelegateTo(
    function: IConsulClientVirtualApiConfiguration
    begin
      Result := TConsulClientVirtualApiConfiguration.Create(
        IniFile.ReadString('Consul', 'URL', 'http://127.0.0.1:8500'),
        IniFile.ReadString('Consul', 'Token', ''),
        True,
        True);
    end).AsSingleton;

  Containers.RegisterJSONClientApi<IConsulAgentServiceApiV1, IConsulClientVirtualApiConfiguration>(Container);
  Containers.RegisterJSONClientApi<IConsulKVStoreApiV1, IConsulClientVirtualApiConfiguration>(Container);

  Container.RegisterType<IConsulRegisterServiceUseCase, TConsulRegisterServiceUseCase>;
  Container.RegisterType<IConsulDeregisterServiceUseCase, TConsulDeregisterServiceUseCase>;

  Container.RegisterType<IConsulKVStoreGetKeyUseCase, TConsulKVStoreGetKeyUseCase>;
  Container.RegisterType<IConsulKVStorePutKeyUseCase, TConsulKVStorePutKeyUseCase>;
  Container.RegisterType<IConsulKVStoreDeleteKeyUseCase, TConsulKVStoreDeleteKeyUseCase>;

  Container.RegisterType<IKVStore, TConsulKVStore>;
  Container.RegisterType<IConsulService>.DelegateTo(
    function: IConsulService
    begin
      Result := TConsulService.Create(
        Container.Resolve<IConsulRegisterServiceUseCase>,
        Container.Resolve<IConsulDeregisterServiceUseCase>,
        nil);
    end);
end;

end.
