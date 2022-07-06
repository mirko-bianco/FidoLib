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

unit Fido.Consul.Service;

interface

uses
  System.SysUtils,

  IdStack,

  Spring.Collections,

  Fido.Utilities,
  Fido.Functional,
  Fido.Functional.Tries,
  Fido.Consul.Service.Intf,
  Fido.Consul.Types,
  Fido.Consul.UseCases.Service.Register.Intf,
  Fido.Consul.UseCases.Service.DeRegister.Intf;

type
  TConsulService = class(TInterfacedObject, IConsulService)
  private
    FConsulRegisterServiceUseCase: IConsulRegisterServiceUseCase;
    FConsulDeregisterServiceUseCase: IConsulDeregisterServiceUseCase;
    FGetIPsFunc: TFunc<TArray<string>>;
    FServiceIds: IList<string>;

    function GetIps: TArray<string>;
    function AddServiceId(const ServiceName, Address: string; const Port: Integer; const ConsulHealthCheck: TConsulHealthCheck; const Timeout: Cardinal): string;
    function DoGetLocalAddresses: Context<Void>.FunctorFunc<TArray<string>>;
  public
    constructor Create(const ConsulRegisterServiceUseCase: IConsulRegisterServiceUseCase; const ConsulDeregisterServiceUseCase: IConsulDeregisterServiceUseCase;
      const GetIPsFunc: TFunc<TArray<string>> = nil);
    destructor Destroy; override;

    procedure Register(const ServiceName: string; const Port: Integer; const HealthEndpoint: string; const Timeout: Cardinal = INFINITE);
    procedure Deregister(const Timeout: Cardinal = INFINITE);
  end;

implementation

{ TConsulService }

constructor TConsulService.Create(
  const ConsulRegisterServiceUseCase: IConsulRegisterServiceUseCase;
  const ConsulDeregisterServiceUseCase: IConsulDeregisterServiceUseCase;
  const GetIPsFunc: TFunc<TArray<string>>);
begin
  inherited Create;

  FGetIPsFunc := GetIps;
  if Assigned(GetIPsFunc) then
    FGetIPsFunc := GetIPsFunc;

  FConsulRegisterServiceUseCase := Utilities.CheckNotNullAndSet(ConsulRegisterServiceUseCase, 'ConsulRegisterServiceUseCase');
  FConsulDeregisterServiceUseCase := Utilities.CheckNotNullAndSet(ConsulDeregisterServiceUseCase, 'ConsulDeregisterServiceUseCase');

  FServiceIds := TCollections.CreateList<string>;
end;

procedure TConsulService.Deregister(const Timeout: Cardinal);
begin
  FServiceIds.ForEach(
    procedure(const ServiceId: string)
    var
      Value: Context<Void>;
    begin
      Value := FConsulDeregisterServiceUseCase.Run(ServiceId, Timeout);
      Value.Value;
    end);

  FServiceIds.Clear;
end;

destructor TConsulService.Destroy;
begin
  Deregister;
  inherited;
end;

function TConsulService.DoGetLocalAddresses: Context<Void>.FunctorFunc<TArray<string>>;
begin
  Result := Void.MapFunc<TArray<string>>(function: TArray<string>
    begin
      Result := GStack.LocalAddresses.ToStringArray;
    end);
end;

function TConsulService.GetIps: TArray<string>;
begin
  GStack.IncUsage;
  Result := &Try<Void>.New(Void.Get).Map<TArray<string>>(DoGetLocalAddresses()).Match(
    procedure
    begin
      GStack.DecUsage;
    end);
end;

function TConsulService.AddServiceId(
  const ServiceName: string;
  const Address: string;
  const Port: Integer;
  const ConsulHealthCheck: TConsulHealthCheck;
  const Timeout: Cardinal): string;
begin
  Result := &Try<string>.
    New(FConsulRegisterServiceUseCase.Run(ServiceName, Address, Port, ConsulHealthCheck, '', Timeout)).
    Match(EConsulService, 'Error while registering to Consul service. Message: %s');
end;

procedure TConsulService.Register(
  const ServiceName: string;
  const Port: Integer;
  const HealthEndpoint: string;
  const Timeout: Cardinal);
var
  Address: string;
  Prot: string;
begin

  Prot := 'http://';
  if Port = 443 then
    Prot := 'https://';

  for Address in FGetIPsFunc do
    FServiceIds.Add(AddServiceId(ServiceName, Address, Port, TConsulHealthCheck.Create(Format('%s%s:%d%s', [Prot, Address, Port, HealthEndpoint])), Timeout));
end;

end.
