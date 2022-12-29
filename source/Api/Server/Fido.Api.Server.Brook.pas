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

unit Fido.Api.Server.Brook;

interface

uses
  System.SysUtils,

  BrookHttpServer,
  BrookHttpRequest,
  BrookHttpResponse,

  Fido.Api.Server.Intf,
  Fido.Http.Request.Intf,
  Fido.Http.RequestInfo.Intf,
  Fido.Http.Request,
  Fido.Http.Response.Intf,
  Fido.Http.ResponseInfo.Intf,
  Fido.Http.Response,
  Fido.Http.Types,
  Fido.Http.Utils,
  Fido.Web.Server.Intf,

  Fido.Api.Server.Abstract,
  Fido.DesignPatterns.Adapter.TBrookHTTPRequestAsIHTTPRequest,
  Fido.DesignPatterns.Adapter.TBrookHTTPResponseAsIHTTPResponse;

type
  {$M+}
  TBrookApiServerRequestFactory = reference to function(const Request: TBrookHTTPRequest): IHttpRequest;

  TBrookApiServerResponseFactory = reference to function(const Response: TBrookHTTPResponse): IHttpResponse;
  {$M-}

  EBrookApiServer = class(EFidoApiException);

  TBrookApiServer = class(TAbstractApiServer, IApiServer)
  private
    FHttpServer: TBrookHTTPServer;
    FApiServerRequestFactory: TBrookApiServerRequestFactory;
    FApiServerResponseFactory: TBrookApiServerResponseFactory;
    FResponseMimeType: TMimeType;
  protected
    function GetDefaultApiRequestFactory: TBrookApiServerRequestFactory;
    function GetDefaultApiResponseFactory: TBrookApiServerResponseFactory;
    procedure OnHTTPCommandEvent(Sender: TObject; Request: TBrookHTTPRequest; Response: TBrookHTTPResponse);
    procedure OnCommandError(Sender: TObject; Request: TBrookHTTPRequest; Response: TBrookHTTPResponse; Exception: Exception);
  public
    constructor Create(const Port: Word; const MaxConnections: Integer; const Threaded: Boolean; const ThreadPoolSize: Cardinal; const ResponseMimeType: TMimeType; const WebServer: IWebServer;
      const SSLCertData: TSSLCertData; const ApiRequestFactory: TBrookApiServerRequestFactory = nil; const ApiResponseFactory: TBrookApiServerResponseFactory = nil);
    destructor Destroy; override;

    function IsActive: Boolean; override;
    procedure SetActive(const Value: Boolean); override;
  end;

implementation

{ TBrookApiServer }

constructor TBrookApiServer.Create(
  const Port: Word;
  const MaxConnections: Integer;
  const Threaded: Boolean;
  const ThreadPoolSize: Cardinal;
  const ResponseMimeType: TMimeType;
  const WebServer: IWebServer;
  const SSLCertData: TSSLCertData;

  const ApiRequestFactory: TBrookApiServerRequestFactory;
  const ApiResponseFactory: TBrookApiServerResponseFactory);
var
  UseSSL: Boolean;
begin
  inherited Create(Port, WebServer, SSLCertData);

  FApiServerRequestFactory := ApiRequestFactory;
  FApiServerResponseFactory := ApiResponseFactory;
  if not Assigned(FApiServerRequestFactory) then
    FApiServerRequestFactory := GetDefaultApiRequestFactory();
  if not Assigned(FApiServerResponseFactory) then
    FApiServerResponseFactory := GetDefaultApiResponseFactory();

  UseSSL := SSLCertData.IsValid;
  FResponseMimeType := ResponseMimeType;

  FHttpServer := TBrookHTTPServer.Create(nil);
  FHttpServer.ConnectionLimit := MaxConnections;
  FHttpServer.Threaded := Threaded;
  FHttpServer.ThreadPoolSize := ThreadPoolSize;
  FHttpServer.Authenticated := False;
  FHttpServer.NoFavicon := True;
  FHttpServer.Port := Port;
  FHttpServer.OnRequest := OnHTTPCommandEvent;
  FHttpServer.OnRequestError := OnCommandError;
  FHttpServer.Security.Active := False;
  if UseSSL then
  begin
    FHttpServer.Security.Active := True;
    FHttpServer.Security.PrivateKey := SSLCertData.SSLKeyFilePath;
    FHttpServer.Security.PrivatePassword := SSLCertData.Password;
    FHttpServer.Security.Certificate := SSLCertData.SSLCertFilePath;
  end;
  FHttpServer.Active := False;
end;

destructor TBrookApiServer.Destroy;
begin
  FHttpServer.Active := False;
  FHttpServer.Free;
  inherited;
end;

function TBrookApiServer.GetDefaultApiRequestFactory: TBrookApiServerRequestFactory;
begin
  Result := function(const Request: TBrookHTTPRequest): IHttpRequest
    begin
      Result := TBrookHTTPRequestAsIHTTPRequestDecorator.Create(Request);
    end;
end;

function TBrookApiServer.GetDefaultApiResponseFactory: TBrookApiServerResponseFactory;
begin
  Result := function(const Response: TBrookHTTPResponse): IHttpResponse
    begin
      Result := TBrookHTTPResponseAsIHTTPResponseDecorator.Create(Response, FResponseMimeType);
    end;
end;

function TBrookApiServer.IsActive: Boolean;
begin
  Result := FHttpServer.Active;
end;

procedure TBrookApiServer.OnCommandError(Sender: TObject; Request: TBrookHTTPRequest; Response: TBrookHTTPResponse; Exception: Exception);
begin
  DoFormatExceptionToResponse(Exception, FApiServerResponseFactory(Response));
end;

procedure TBrookApiServer.OnHTTPCommandEvent(
  Sender: TObject;
  Request: TBrookHTTPRequest;
  Response: TBrookHTTPResponse);
var
  ApiRequest: IHttpRequest;
  ApiResponse: IHttpResponse;
begin
  ApiRequest := FApiServerRequestFactory(Request);
  ApiResponse := FApiServerResponseFactory(Response);

  ProcessCommand(ApiRequest, ApiResponse);
end;

procedure TBrookApiServer.SetActive(const Value: Boolean);
begin
  FHttpServer.Active := Value;
end;

end.
