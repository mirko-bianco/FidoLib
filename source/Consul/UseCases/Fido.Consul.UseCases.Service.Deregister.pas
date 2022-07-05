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

unit Fido.Consul.UseCases.Service.Deregister;

interface

uses
  Spring.Collections,

  Fido.Functional,
  Fido.Utilities,
  Fido.Consul.Gateways.Service.Intf,
  Fido.Consul.UseCases.Service.Deregister.Intf;

type
  TConsulDeregisterServiceUseCase = class(TInterfacedObject, IConsulDeregisterServiceUseCase)
  private
    FConsulServiceApiGateway: IConsulServiceApiGateway;
  public
    constructor Create(const ConsulServiceApiGateway: IConsulServiceApiGateway);

    function Run(const ServiceId: string; const Timeout: Cardinal = INFINITE): Context<Void>;
  end;

implementation

{ TConsulDeregisterServiceUseCase }

constructor TConsulDeregisterServiceUseCase.Create(const ConsulServiceApiGateway: IConsulServiceApiGateway);
begin
  inherited Create;
  FConsulServiceApiGateway := Utilities.CheckNotNullAndSet(ConsulServiceApiGateway, 'ConsulServiceApiGateway');
end;

function TConsulDeregisterServiceUseCase.Run(
  const ServiceId: string;
  const Timeout: Cardinal): Context<Void>;
begin
  Result := FConsulServiceApiGateway.Deregister(ServiceId, Timeout);
end;

end.

