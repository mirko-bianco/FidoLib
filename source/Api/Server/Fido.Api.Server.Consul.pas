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

unit Fido.Api.Server.Consul;

interface

uses
  System.SysUtils,
  System.Rtti,

  IdStack,

  Spring,
  Spring.Collections,

  Fido.Api.Server.Resource.Attributes,
  Fido.Api.Server.Intf,

  Fido.Consul.Service.Intf,
  Fido.Api.Server.Consul.Resource.Attributes;

type
  TConsulAwareApiServer = class(TInterfacedObject, IApiServer)
  private
    FApiServer: IApiServer;
    FConsulService: IConsulService;
    FServiceName: string;
    FHealthEndpoint: string;
  public
    constructor Create(const ApiServer: IApiServer; const ConsulService: IConsulService; const ServiceName: string);

    Function Port: Word;
    function IsActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure RegisterResource(const Resource: TObject);
    procedure RegisterWebSocket(const WebSocketClass: TClass);
    procedure RegisterRequestMiddleware(const Name: string; const Step: TRequestMiddlewareFunc);
    procedure RegisterResponseMiddleware(const Name: string; const Step: TResponseMiddlewareProc);
  end;

implementation

{ TConsulAwareApiServer }

constructor TConsulAwareApiServer.Create(
  const ApiServer: IApiServer;
  const ConsulService: IConsulService;
  const ServiceName: string);
begin
  inherited Create;
  Guard.CheckNotNull(ApiServer, 'ApiServer');
  Guard.CheckNotNull(ConsulService, 'ConsulService');

  FApiServer := ApiServer;
  FConsulService := ConsulService;
  FServiceName := ServiceName;
  FHealthEndpoint := '';
end;

function TConsulAwareApiServer.IsActive: Boolean;
begin
  Result := FApiServer.IsActive;
end;

function TConsulAwareApiServer.Port: Word;
begin
  Result := FApiServer.Port;
end;

procedure TConsulAwareApiServer.RegisterRequestMiddleware(
  const Name: string;
  const Step: TRequestMiddlewareFunc);
begin
  FApiServer.RegisterRequestMiddleware(Name, Step);
end;

procedure TConsulAwareApiServer.RegisterResource(const Resource: TObject);
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
  Attr: TCustomAttribute;
  Path: string;
  BaseUrl: string;
begin
  FApiServer.RegisterResource(Resource);

  Ctx := TRttiContext.Create;
  RttiType := Ctx.GetType(Resource.ClassType);

  for Attr in RttiType.GetAttributes do
    if Attr is BaseUrlAttribute then
      BaseUrl := (Attr as BaseUrlAttribute).BaseUrl;

  for RttiMethod in RttiType.GetMethods do
  begin
    Path := '';
    for Attr in RttiMethod.GetAttributes do
      if Attr is PathAttribute then
      begin
        Path := (Attr as PathAttribute).Path;
        if Path.Contains('/{') then
          Path := Copy(Path, 1, Pos('/{', Path) - 1)
        else if Path.Contains('{') then
          Path := Copy(Path, 1, Pos('{', Path) - 1);
      end;

    if Path.IsEmpty then
      Continue;

    for Attr in RttiMethod.GetAttributes do
      if Attr is ConsulHealthCheckAttribute then
        FHealthEndpoint := Format('%s%s', [BaseUrl, Path]);
  end;
end;

procedure TConsulAwareApiServer.RegisterResponseMiddleware(
  const Name: string;
  const Step: TResponseMiddlewareProc);
begin
  FApiServer.RegisterResponseMiddleware(Name, Step);
end;

procedure TConsulAwareApiServer.RegisterWebSocket(const WebSocketClass: TClass);
begin
  FApiServer.RegisterWebSocket(WebSocketClass);
end;

procedure TConsulAwareApiServer.SetActive(const Value: Boolean);
begin
  FApiServer.SetActive(Value);
  case Value of
    True: FConsulService.Register(FServiceName, FApiServer.Port, FHealthEndpoint);
    False: FConsulService.Deregister;
  end;
end;

end.
