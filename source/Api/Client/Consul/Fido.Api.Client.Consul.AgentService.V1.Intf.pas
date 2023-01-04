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

unit Fido.Api.Client.Consul.AgentService.V1.Intf;

interface

uses
  System.SysUtils,

  Spring.Collections,

  Fido.Http.Types,
  Fido.Api.Client.VirtualApi.Attributes,
  Fido.Api.Client.VirtualApi.Intf,
  Fido.Consul.Types,
  Fido.Api.Client.Consul.Constants;

type
  TConsulService = record
  private
    FId: string;
    FServiceName: string;
    FAddress: string;
    FPort: Integer;
    FCheck: TConsulHealthCheck;
    FTags: IReadOnlyList<string>;
  public
    constructor Create(const ServiceName: string; const Address: string; const Port: Integer; const Check: TConsulHealthCheck; const Tags: TArray<string>; const Id: string);

    function Id: string;
    function Name: string;
    function Address: string;
    function Port: Integer;
    function Check: TConsulHealthCheck;
    function Tags: IReadonlyList<string>;
  end;

  IConsulAgentServiceApiV1 = interface(IClientVirtualApi)
    ['{116D716C-3334-4C6D-AEF4-4277AF9CE911}']

    [Endpoint(rmPut, '/v1/agent/service/register')]
    [RequestParam('ConsulService')]
    [HeaderParam('Token', CONSUL_TOKEN)]
    procedure Register(const ConsulService: TConsulService);

    [Endpoint(rmPut, '/v1/agent/service/deregister/{ServiceId}')]
    [HeaderParam('Token', CONSUL_TOKEN)]
    procedure Deregister(const ServiceId: string);
  end;

implementation

{ TConsulService }

function TConsulService.Address: string;
begin
  Result := FAddress;
end;

function TConsulService.Check: TConsulHealthCheck;
begin
  Result := FCheck;
end;

constructor TConsulService.Create(
  const ServiceName: string;
  const Address: string;
  const Port: Integer;
  const Check: TConsulHealthCheck;
  const Tags: TArray<string>;
  const Id: string);
begin
  FId := Id;
  FServiceName := ServiceName;
  FAddress := Address;
  FPort := Port;
  FTags := TCollections.CreateList<string>(Tags).AsReadOnly;
  FCheck := Check;
end;

function TConsulService.Id: string;
begin
  Result := FId;
end;

function TConsulService.Name: string;
begin
  Result := FServiceName;
end;

function TConsulService.Port: Integer;
begin
  Result := FPort;
end;

function TConsulService.Tags: IReadonlyList<string>;
begin
  Result := FTags;
end;

end.

