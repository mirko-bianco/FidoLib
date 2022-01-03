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

unit Fido.Consul.UseCases.Service.Register;

interface

uses
  System.SysUtils,

  Spring,
  Spring.Collections,

  Fido.Consul.Types,
  Fido.Api.Client.Consul.AgentService.V1.Intf,
  Fido.Consul.UseCases.Service.Register.Intf;

type
  TConsulRegisterServiceUseCase = class(TInterfacedObject, IConsulRegisterServiceUseCase)
  private
    FRegisterServiceApi: IConsulAgentServiceApiV1;
  public
    constructor Create(const RegisterServiceApi: IConsulAgentServiceApiV1);

    function Run(const ServiceName: string; const Address: string; const Port: Integer; const HealthCheck: TConsulHealthCheck; const ServiceId: string = ''): string;
  end;

implementation

{ TConsulRegisterServiceUseCase }

constructor TConsulRegisterServiceUseCase.Create(const RegisterServiceApi: IConsulAgentServiceApiV1);
begin
  inherited Create;
  Guard.CheckNotNull(RegisterServiceApi, 'RegisterServiceApi');
  FRegisterServiceApi := RegisterServiceApi;
end;

function TConsulRegisterServiceUseCase.Run(
  const ServiceName: string;
  const Address: string;
  const Port: Integer;
  const HealthCheck: TConsulHealthCheck;
  const ServiceId: string): string;
var
  LServiceId: string;
  LTags: TArray<string>;
begin
  SetLength(LTags, 1);
  LTags[0] := Format('urlprefix-/%s', [ServiceName.ToLower]);

  LServiceId := ServiceId;
  if LServiceId.IsEmpty then
    LServiceId := Format('%s-%s-%d', [ServiceName, Address, Port]);

  FRegisterServiceApi.Register(
    TConsulService.Create(
      ServiceName,
      Address,
      Port,
      HealthCheck,
      LTags,
      LServiceId));

  Result := LServiceId;
end;

end.

