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

  Spring.Collections,

  Fido.Utilities,
  Fido.DesignPatterns.Retries,
  Fido.Functional,
  Fido.Functional.Retries,
  Fido.Functional.Ifs,
  Fido.Functional.Tries,
  Fido.Consul.Types,
  Fido.Api.Client.Consul.AgentService.V1.Intf,
  Fido.Consul.Gateways.Service.Intf,
  Fido.Consul.UseCases.Service.Register.Intf;

type
  TConsulRegisterServiceUseCase = class(TInterfacedObject, IConsulRegisterServiceUseCase)
  private
    FRegisterServiceApiGateway: IConsulServiceApiGateway;

    function DoRegister(const Timeout: Cardinal): Context<TConsulService>.MonadFunc<Void>;
  public
    constructor Create(const RegisterServiceApiGateway: IConsulServiceApiGateway);

    function Run(const ServiceName: string; const Address: string; const Port: Integer; const HealthCheck: TConsulHealthCheck; const ServiceId: string = ''; const Timeout: Cardinal = INFINITE): Context<string>;
  end;

implementation

{ TConsulRegisterServiceUseCase }

constructor TConsulRegisterServiceUseCase.Create(const RegisterServiceApiGateway: IConsulServiceApiGateway);
begin
  inherited Create;
  FRegisterServiceApiGateway := Utilities.CheckNotNullAndSet(RegisterServiceApiGateway, 'RegisterServiceApiGateway');
end;

function TConsulRegisterServiceUseCase.DoRegister(const Timeout: Cardinal): Context<TConsulService>.MonadFunc<Void>;
var
  Gateway: IConsulServiceApiGateway;
begin
  Gateway := FRegisterServiceApiGateway;

  Result := function(const Service: TConsulService): Context<Void>
    begin
      Result := Gateway.Register(Service, Timeout);
    end;
end;

function TConsulRegisterServiceUseCase.Run(
  const ServiceName: string;
  const Address: string;
  const Port: Integer;
  const HealthCheck: TConsulHealthCheck;
  const ServiceId: string;
  const Timeout: Cardinal): Context<string>;
var
  LServiceId: string;
  LTags: TArray<string>;

  ConsulService: TConsulService;
  BoolContext: Context<Boolean>;
begin
  SetLength(LTags, 1);
  LTags[0] := Format('urlprefix-/%s', [ServiceName.ToLower]);

  LServiceId := ServiceId;
  if LServiceId.IsEmpty then
    LServiceId := Format('%s-%s-%d', [ServiceName, Address, Port]);

  ConsulService := TConsulService.Create(ServiceName, Address, Port, HealthCheck, LTags, LServiceId);

  BoolContext := &Try<Void>.New(Retry<TConsulService>.New(ConsulService).Map<Void>(DoRegister(Timeout), Retries.GetRetriesOnExceptionFunc())).Match;

  Result := ThenElse.New(BoolContext).&Then<string>('', LServiceId);
end;

end.

