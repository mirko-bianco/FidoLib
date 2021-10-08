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

unit Fido.Api.Client.VirtualApi.json;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Rtti,
  System.Json,
  System.TypInfo,
  System.Generics.Collections,
  Rest.Types,
  Rest.Client,

  Spring,
  Spring.Collections,

  Fido.Api.Client.VirtualApi.Abstract,
  Fido.Api.Client.VirtualApi.Intf,
  Fido.Json.Marshalling,
  Fido.Api.Client.VirtualApi.Request,
  Fido.Api.Client.VirtualApi.Configuration.Intf,
  Fido.Api.Client.VirtualApi.Call;

type
  {$M+}
  TJSONClientVirtualApi<T: IClientVirtualApi; IConfiguration: IClientVirtualApiConfiguration> = class(TAbstractClientVirtualApi<T, IConfiguration>)
  protected
    function ConvertTValueToString(const Value: TValue): string; override;
    function ConvertResponseToDto(const Response: string; const TypeInfo: PTypeInfo): TValue; override;
    function ConvertRequestDtoToString(const Value: TValue): string; override;
    procedure CallApi(const Call: TClientVirtualApiCall); override;
  end;

implementation

procedure TJSONClientVirtualApi<T, IConfiguration>.CallApi(const Call: TClientVirtualApiCall);
var
  ApiClient: Shared<TRestClient>;
  ApiRequest: Shared<TApiClientVirtualApiRequest>;
  ApiResponse: Shared<TRestResponse>;
  Pair: TPair<string, string>;
  Parameter: TClientVirtualApiCallParameter;
begin
  inherited;

  Guard.CheckNotNull(Call, 'Call');
  Guard.CheckNotNull(Call.ResponseHeaders, 'Call.ResponseHeaders');

  ApiClient := TRestClient.Create(nil);
  ApiRequest := TApiClientVirtualApiRequest.Create(nil);
  ApiResponse := TRestResponse.Create(nil);

  ApiClient.Value.Accept := GetAcceptHeaderWithApiVersion(Call.ContentType);
  ApiClient.Value.ContentType := Call.ContentType;
  ApiClient.Value.BaseURL := Call.Url;

  ApiRequest.Value.Client := ApiClient;
  ApiRequest.Value.Response := ApiResponse;
  ApiRequest.Value.Method := Call.ApiMethod;
  ApiRequest.Value.Timeout := Call.Timeout;

  for Parameter in Call.Parameters do
  begin
    Guard.CheckFalse(Parameter.Value.IsEmpty);
    case Parameter.Kind of
      pkQuery,
      pkForm:
        ApiRequest.Value.AddParameter(Parameter.Name, Parameter.Value);
      pkHeader:
        ApiRequest.Value.Params.AddHeader(Parameter.Name, Parameter.Value);
      pkFile:
        ; // unimplemented
    end;
  end;

  if not Call.PostBody.IsEmpty then
    ApiRequest.Value.AddBody(Call.PostBody, ctAPPLICATION_JSON);

  try
    ApiRequest.Value.HandleRedirects := Call.HandleRedirects;
    ApiRequest.Value.Execute;
  except
  end;

  Call.Finish(ApiResponse.Value.StatusCode, ApiResponse.Value.Content, ApiResponse.Value.Headers);
end;

function TJSONClientVirtualApi<T, IConfiguration>.ConvertRequestDtoToString(const Value: TValue): string;
begin
  inherited;
  Result := JSONMarshaller.From(Value.AsObject, Value.TypeInfo);
end;

function TJSONClientVirtualApi<T, IConfiguration>.ConvertResponseToDto(const Response: string; const TypeInfo: PTypeInfo): TValue;
begin
  inherited;

  if Response.IsEmpty then
    Exit(nil);

  Result := JSONUnmarshaller.&To(Response, TypeInfo);
end;

function TJSONClientVirtualApi<T, IConfiguration>.ConvertTValueToString(const Value: TValue): string;
begin
  inherited;
  Result := JSONMarshaller.From(Value, Value.TypeInfo).DeQuotedString('"');
end;

end.
