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

unit Fido.Consul.Gateways.Service;

interface

uses
  Generics.Collections,

  Spring.Collections,

  Fido.Utilities,
  Fido.Functional,
  Fido.Api.Client.Consul.AgentService.V1.Intf,
  Fido.Consul.Gateways.Service.Intf;

type
  TConsulServiceApiGateway = class(TInterfacedObject, IConsulServiceApiGateway)
  private
    FConsulAgentServiceApiV1: IConsulAgentServiceApiV1;
    procedure DoRegister(const ConsulService: TConsulService);
    procedure DoDeregister(const ServiceId: string);
  public
    constructor Create(const ConsulAgentServiceApiV1: IConsulAgentServiceApiV1);

    function Register(const ConsulService: TConsulService; const Timeout: Cardinal = INFINITE): Context<Void>;
    function Deregister(const ServiceId: string; const Timeout: Cardinal = INFINITE): Context<Void>;
  end;

implementation

{ TConsulServiceApiGateway }

constructor TConsulServiceApiGateway.Create(const ConsulAgentServiceApiV1: IConsulAgentServiceApiV1);
begin
  inherited Create;

  FConsulAgentServiceApiV1 := Utilities.CheckNotNullAndSet<IConsulAgentServiceApiV1>(ConsulAgentServiceApiV1, 'ConsulAgentServiceApiV1');
end;

procedure TConsulServiceApiGateway.DoRegister(const ConsulService: TConsulService);
begin
  FConsulAgentServiceApiV1.Register(ConsulService);
end;

function TConsulServiceApiGateway.Register(
  const ConsulService: TConsulService;
  const Timeout: Cardinal = INFINITE): Context<Void>;
begin
  Result := Context<TConsulService>.New(ConsulService).MapAsync<Void>(Void.MapProc<TConsulService>(DoRegister), Timeout);
end;

procedure TConsulServiceApiGateway.DoDeregister(const ServiceId: string);
begin
  FConsulAgentServiceApiV1.Deregister(ServiceId);
end;

function TConsulServiceApiGateway.Deregister(
  const ServiceId: string;
  const Timeout: Cardinal = INFINITE): Context<Void>;
begin
  Result := Context<string>.New(ServiceId).MapAsync<Void>(Void.MapProc<string>(DoDeregister), Timeout);
end;

end.

