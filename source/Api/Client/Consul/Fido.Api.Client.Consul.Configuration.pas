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

unit Fido.Api.Client.Consul.Configuration;

interface

uses
  Fido.Api.Client.VirtualApi.Attributes,
  Fido.Api.Client.VirtualApi.Intf,
  Fido.Api.Client.VirtualApi.Configuration.Intf,
  Fido.Api.Client.VirtualApi.Configuration,

  Fido.Api.Client.Consul.Constants;

type
  {$M+}
  IConsulClientVirtualApiConfiguration = interface(IClientVirtualApiConfiguration)
    ['{53890928-A51D-479A-95C2-1BF582D5C9B8}']

    [ApiParam(CONSUL_TOKEN)]
    function GetToken: string;
  end;

  TConsulClientVirtualApiConfiguration = class(TClientVirtualApiConfiguration, IConsulClientVirtualApiConfiguration)
  private
    FToken: string;
  public
    constructor Create(const BaseUrl: string; const Token: string; const Active: Boolean; const LiveEnvironment: Boolean);

    function GetToken: string;
  end;

implementation

{ TConsulClientVirtualApiConfiguration }

constructor TConsulClientVirtualApiConfiguration.Create(
  const BaseUrl: string;
  const Token: string;
  const Active: Boolean;
  const LiveEnvironment: Boolean);
begin
  inherited Create(BaseUrl, Active, LiveEnvironment);

  FToken := Token;
end;

function TConsulClientVirtualApiConfiguration.GetToken: string;
begin
  Result := FToken;
end;

end.

