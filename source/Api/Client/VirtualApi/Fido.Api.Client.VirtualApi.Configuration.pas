(*
 * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
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

unit Fido.Api.Client.VirtualApi.Configuration;

interface

uses
  System.SysUtils,
  Fido.Api.Client.VirtualApi.Configuration.Intf,
  Fido.Api.Client.VirtualApi.Call;

type
  TClientVirtualApiConfiguration = class(TInterfacedObject, IClientVirtualApiConfiguration)
  private
    FBaseUrl: string;
    FActive: Boolean;
    FLiveEnvironment: Boolean;
  public
    constructor Create(
      const BaseUrl: string;
      const Active: Boolean;
      const LiveEnvironment: Boolean);

    // IClientVirtualApiConfiguration
    function BaseUrl: string;
    function Active: Boolean;
    function LiveEnvironment: Boolean;
  end;

  TActiveVirtualApiConfiguration = class(TClientVirtualApiConfiguration, IClientVirtualApiConfiguration, IActiveClientVirtualApiConfiguration)
  public
    // IActiveClientVirtualApiConfiguration
    procedure CallBegins(const Call: TClientVirtualApiCall); virtual;
    procedure CallEnded(const Call: TClientVirtualApiCall); virtual;
  end;

implementation

{ TClientVirtualApiConfiguration }

function TClientVirtualApiConfiguration.Active: Boolean;
begin
  Result := FActive;
end;

function TClientVirtualApiConfiguration.BaseUrl: string;
begin
  Result := FBaseUrl;
end;

constructor TClientVirtualApiConfiguration.Create(const BaseUrl: string; const Active: Boolean; const LiveEnvironment: Boolean);
begin
  FBaseUrl := BaseUrl;
  FActive := Active;
  FLiveEnvironment := LiveEnvironment;
end;

function TClientVirtualApiConfiguration.LiveEnvironment: Boolean;
begin
  Result := FLiveEnvironment;
end;

{ TActiveApiVirtualApiConfiguration }

procedure TActiveVirtualApiConfiguration.CallBegins(const Call: TClientVirtualApiCall);
begin
end;

procedure TActiveVirtualApiConfiguration.CallEnded(const Call: TClientVirtualApiCall);
begin
end;

end.
