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

  Spring,
  Spring.Collections,

  Fido.Consul.Service.Intf,
  Fido.Consul.Types,
  Fido.Consul.UseCases.Service.Register.Intf,
  Fido.Consul.UseCases.Service.DeRegister.Intf;

type
  TConsulService = class(TInterfacedObject, IConsulService)
  private
    FConsulRegisterServiceUseCase: IConsulRegisterServiceUseCase;
    FConsulDeregisterServiceUseCase: IConsulDeregisterServiceUseCase;
    FServiceIds: IList<string>;

    function GetIps: TArray<string>;
  public
    constructor Create(const ConsulRegisterServiceUseCase: IConsulRegisterServiceUseCase; const ConsulDeregisterServiceUseCase: IConsulDeregisterServiceUseCase);
    destructor Destroy; override;

    procedure Register(const ServiceName: string; const Port: Integer; const HealthEndpoint: string);
    procedure Deregister;
  end;

implementation

{ TConsulService }

constructor TConsulService.Create(
  const ConsulRegisterServiceUseCase: IConsulRegisterServiceUseCase;
  const ConsulDeregisterServiceUseCase: IConsulDeregisterServiceUseCase);
begin
  inherited Create;

  Guard.CheckNotNull(ConsulRegisterServiceUseCase, 'ConsulRegisterServiceUseCase');
  Guard.CheckNotNull(ConsulDeregisterServiceUseCase, 'ConsulDeregisterServiceUseCase');

  FConsulRegisterServiceUseCase := ConsulRegisterServiceUseCase;
  FConsulDeregisterServiceUseCase := ConsulDeregisterServiceUseCase;

  FServiceIds := TCollections.CreateList<string>;
end;

procedure TConsulService.Deregister;
begin
  FServiceIds.ForEach(
    procedure(const ServiceId: string)
    begin
      FConsulDeregisterServiceUseCase.Run(ServiceId);
    end);

  FServiceIds.Clear;
end;

destructor TConsulService.Destroy;
begin
  Deregister;
  inherited;
end;

function TConsulService.GetIps: TArray<string>;
begin
  GStack.IncUsage;
  try
    Result := GStack.LocalAddresses.ToStringArray;
  finally
    GStack.DecUsage;
  end;
end;

procedure TConsulService.Register(
  const ServiceName: string;
  const Port: Integer;
  const HealthEndpoint: string);
var
  Address: string;
  Prot: string;
begin
  Prot := 'http://';
  if Port = 443 then
    Prot := 'https://';

  for Address in GetIps do
    try
      FServiceIds.Add(
        FConsulRegisterServiceUseCase.Run(
          ServiceName,
          Address,
          Port,
          TConsulHealthCheck.Create(Format('%s%s:%d%s', [Prot, Address, Port, HealthEndpoint]))));
    except
      on E: Exception do
        raise EConsulService.Create(Format('Error while registering to Consul service. Message: %s', [E.Message]));
    end;
end;

end.
