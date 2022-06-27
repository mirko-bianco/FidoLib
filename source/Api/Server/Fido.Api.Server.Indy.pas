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

unit Fido.Api.Server.Indy;

interface

uses
  System.SysUtils,

  idHttpServer,
  idContext,
  IdCustomHTTPServer,
  IdGlobal,
  IdGlobalProtocols,
  IdURI,
  IdSSLOpenSSL,

  Fido.Api.Server.Intf,
  Fido.Http.Request.Intf,
  Fido.Http.RequestInfo.Intf,
  Fido.Http.Request,
  Fido.Http.Response.Intf,
  Fido.Http.ResponseInfo.Intf,
  Fido.Http.Response,
  Fido.Http.Types,
  Fido.Web.Server.Intf,
  Fido.Web.Server.Http.Indy,

  Fido.Api.Server.Abstract,
  Fido.DesignPatterns.Adapter.TIdHTTPRequestInfoAsIHTTPRequestInfo,
  Fido.DesignPatterns.Adapter.TIdHTTPResponseInfoAsIHTTPResponseInfo;

type
  {$M+}
  TIndyApiServerRequestFactory = reference to function(const RequestInfo: TIdHTTPRequestInfo): IHttpRequest;

  TIndyApiServerResponseFactory = reference to function(const Context: TIdContext; const RequestInfo: TIdHTTPRequestInfo; const ResponseInfo: TIdHTTPResponseInfo): IHttpResponse;
  {$M-}

  EIndyApiServer = class(EFidoApiException);

  TIndyApiServer = class(TAbstractApiServer<TIndyApiServerRequestFactory, TIndyApiServerResponseFactory>, IApiServer)
  private
    FHttpServer: TFidoIndyHTTPServer;
    FSSLIOHandler: TIdServerIOHandlerSSLOpenSSL;

    function ConvertSSLVersion(const SSLVersion: TSSLVersion): TIdSSLVersion;
  protected
    function GetDefaultApiRequestFactory: TIndyApiServerRequestFactory; override;
    function GetDefaultApiResponseFactory: TIndyApiServerResponseFactory; override;
    procedure OnParseAuthentication(Context: TIdContext; const AuthType: string; const AuthData: string; var Username: string; var Password: string; var Handled: Boolean);
    procedure OnHTTPCommandEvent(Context: TIdContext; RequestInfo: TIdHTTPRequestInfo; ResponseInfo: TIdHTTPResponseInfo);
    function OnVerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth: integer; AError: Integer): Boolean;
    procedure OnQuerySSLPort(APort: Word; var VUseSSL: Boolean);
  public
    constructor Create(const Port: Word; const MaxConnections: Integer; const WebServer: IWebServer; const SSLCertData: TSSLCertData; const ApiRequestFactory: TIndyApiServerRequestFactory = nil;
      const ApiResponseFactory: TIndyApiServerResponseFactory = nil);
    destructor Destroy; override;

    function IsActive: Boolean; override;
    procedure SetActive(const Value: Boolean); override;
  end;

implementation

{ TIndyApiServer }

function TIndyApiServer.ConvertSSLVersion(const SSLVersion: TSSLVersion): TIdSSLVersion;
begin
  case SSLVersion of
    SSLv2: Result := sslvSSLv2;
    SSLv23: Result := sslvSSLv23;
    SSLv3: Result := sslvSSLv3;
    TLSv1: Result := sslvTLSv1;
    TLSv1_1: Result := sslvTLSv1_1;
    TLSv1_2: Result := sslvTLSv1_2;
  else
    raise EIndyApiServer.Create('SSL version not supported');
  end;
end;

constructor TIndyApiServer.Create(
  const Port: Word;
  const MaxConnections: Integer;
  const WebServer: IWebServer;
  const SSLCertData: TSSLCertData;
  const ApiRequestFactory: TIndyApiServerRequestFactory;
  const ApiResponseFactory: TIndyApiServerResponseFactory);
var
  UseSSL: Boolean;
begin
  inherited;
  UseSSL := SSLCertData.IsValid;

  FPort := Port;
  if UseSSL then
  begin
    FSSLIOHandler := TIdServerIOHandlerSSLOpenSSL.Create(nil);
    FSSLIOHandler.SSLOptions.RootCertFile := SSLCertData.SSLRootCertFilePath;
    FSSLIOHandler.SSLOptions.CertFile := SSLCertData.SSLCertFilePath;
    FSSLIOHandler.SSLOptions.KeyFile := SSLCertData.SSLKeyFilePath;
    FSSLIOHandler.SSLOptions.Method := ConvertSSLVersion(SSLCertData.SSLVersion);
    FSSLIOHandler.SSLOptions.Mode := sslmUnassigned;
    FSSLIOHandler.OnGetPassword := nil;
    FSSLIOHandler.OnVerifyPeer := OnVerifyPeer;
  end;

  FHttpServer := TFidoIndyHTTPServer.Create(nil);
  FHttpServer.OnCommandGet := OnHTTPCommandEvent;
  FHttpServer.OnCommandOther := OnHTTPCommandEvent;
  FHttpServer.OnParseAuthentication := OnParseAuthentication;
  if UseSSL then
  begin
    FHttpServer.IOhandler := FSSLIOHandler;
    FHttpServer.OnQuerySSLPort := OnQuerySSLPort;
    FHttpServer.DefaultPort := SSL_PORT;
  end
  else
    FHttpServer.Bindings.Add.Port := FPort;
  FHttpServer.MaxConnections := MaxConnections;
  FHttpServer.Active := False;
end;

destructor TIndyApiServer.Destroy;
begin
  FHttpServer.Active := False;
  FHttpServer.Free;
  FSSLIOHandler.Free;
  inherited;
end;

function TIndyApiServer.GetDefaultApiRequestFactory: TIndyApiServerRequestFactory;
begin
  Result := function(const RequestInfo: TIdHTTPRequestInfo): IHttpRequest
    var
      RequestInfoDecorator: IHttpRequestInfo;
    begin
      RequestInfoDecorator := TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.Create(RequestInfo);
      Result := THttpRequest.Create(RequestInfoDecorator);
    end;
end;

function TIndyApiServer.GetDefaultApiResponseFactory: TIndyApiServerResponseFactory;
begin
  Result := function(
      const Context: TIdContext;
      const RequestInfo: TIdHTTPRequestInfo;
      const ResponseInfo: TIdHTTPResponseInfo): IHttpResponse
    var
      RequestInfoDecorator: IHttpRequestInfo;
      ResponseInfoDecorator: IHttpResponseInfo;
    begin
      RequestInfoDecorator := TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.Create(RequestInfo);
      ResponseInfoDecorator := TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.Create(Context, RequestInfo, ResponseInfo);
      Result := THttpResponse.Create(RequestInfoDecorator, ResponseInfoDecorator);
    end;
end;

function TIndyApiServer.IsActive: Boolean;
begin
  Result := FHttpServer.Active;
end;

procedure TIndyApiServer.OnHTTPCommandEvent(
  Context: TIdContext;
  RequestInfo: TIdHTTPRequestInfo;
  ResponseInfo: TIdHTTPResponseInfo);
var
  ApiRequest: IHttpRequest;
  ApiResponse: IHttpResponse;
begin
  if RequestInfo.URI.Equals('/favicon.ico') then
    Exit;

  ApiRequest := GetApiRequestFactory()(RequestInfo);
  ApiResponse := GetApiResponseFactory()(Context, RequestInfo, ResponseInfo);

  ProcessCommand(ApiRequest, ApiResponse);
end;

procedure TIndyApiServer.OnParseAuthentication(
  Context: TIdContext;
  const AuthType: string;
  const AuthData: string;
  var Username: string;
  var Password: string;
  var Handled: Boolean);
begin
  Handled := True;
end;

procedure TIndyApiServer.OnQuerySSLPort(
  APort: Word;
  var VUseSSL: Boolean);
begin
  VUseSSL := (APort = SSL_PORT);
end;

function TIndyApiServer.OnVerifyPeer(
  Certificate: TIdX509;
  AOk: Boolean;
  ADepth, AError: Integer): Boolean;
begin
  result := AOk;
end;

procedure TIndyApiServer.SetActive(const Value: Boolean);
begin
  FHttpServer.Active := Value;
end;

end.
