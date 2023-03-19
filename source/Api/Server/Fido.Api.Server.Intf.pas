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

unit Fido.Api.Server.Intf;

interface

uses
  System.SysUtils,

  Spring,
  Spring.Logging,

  Fido.Http.Types,
  Fido.Http.Request.Intf,
  Fido.Http.Response.Intf,
  Fido.JSON.Marshalling,
  Fido.Web.Server.Intf,
  Fido.Api.Server.Exceptions;

type
  {$M+}
  TApiGlobalMiddlewareProc = reference to procedure(const EndpointMethod: Action; const ClassName: string; const MethodName: string);

  TApiRequestMiddlewareFunc = reference to function(const CommaSeparaterParams: string; const ApiRequest: IHttpRequest; out ResponseCode: Integer; out ResponseText: string): Boolean;

  TApiResponseMiddlewareProc = reference to procedure(const CommaSeparaterParams: string; const ApiRequest: IHttpRequest; const ApiResponse: IHttpResponse);

  TApiExceptionMiddlewareProc = reference to procedure(const E: Exception);

  TApiFormatExceptionToResponseProc = reference to procedure(const E: Exception; const ApiResponse: IHttpResponse);
  {$M-}

  IApiServer = interface(IInvokable)
    ['{AA282BB3-418E-4835-8752-73D8DCCD326A}']

    function Port: Word;
    function IsActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetWebServer(const WebServer: IWebServer);
    procedure RegisterResource(const Resource: TObject);
    procedure RegisterRequestMiddleware(const Name: string; const Step: TApiRequestMiddlewareFunc);
    procedure RegisterResponseMiddleware(const Name: string; const Step: TApiResponseMiddlewareProc);
    procedure RegisterExceptionMiddleware(const MiddlewareProc: TApiExceptionMiddlewareProc);
    procedure RegisterGlobalMiddleware(const MiddlewareProc: TApiGlobalMiddlewareProc);
    procedure RegisterFormatExceptionToResponse(const FormatExceptionToResponseProc: TApiFormatExceptionToResponseProc);
  end;

var
  DefaultExceptionMiddlewareProc: TApiExceptionMiddlewareProc;
  DefaultGlobalMiddlewareProc: TApiGlobalMiddlewareProc;
  DefaultFormatExceptionToResponseProc: TApiFormatExceptionToResponseProc;

implementation

initialization
  DefaultExceptionMiddlewareProc := procedure(const E: Exception)
    begin
      if E.InheritsFrom(EJSONUnmarshaller) then
        raise EApiServer400.Create(E.Message);
      if E.InheritsFrom(EJSONMarshaller) then
        raise EApiServer400.Create(E.Message);
      if E.InheritsFrom(EJSONVirtualDto) then
        raise EApiServer400.Create(E.Message);

      raise EApiServer500.Create(E.Message);
    end;

  DefaultGlobalMiddlewareProc := procedure(const EndpointMethod: Action; const ClassName: string; const MethodName: string)
    begin
      EndpointMethod();
    end;

  DefaultFormatExceptionToResponseProc := procedure(const E: Exception; const ApiResponse: IHttpResponse)
    var
      Code: Integer;
      Text: string;
    begin
      if E.InheritsFrom(EApiServer) then
      begin
        Code := (E as EApiServer).Code;
        Text := (E as EApiServer).ShortMsg;
      end
      else
      begin
        Code := 500;
        Text := 'Internal server error';
      end;
      ApiResponse.SetMimeType(mtJson);
      ApiResponse.SetBody(Format('{"Error": {"Code": %d, "Status": "%s", "Message": "%s"}}', [Code, Text, E.Message]));
      ApiResponse.SetResponseCode(Code, Text);
    end;
end.
