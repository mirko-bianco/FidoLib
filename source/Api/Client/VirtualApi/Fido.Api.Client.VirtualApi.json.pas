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
  IdHttp,
  IdSSLOpenSSL,

  Spring,
  Spring.Collections,

  Fido.Utilities,
  Fido.Http.Types,
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
    function ConvertRawRequestDtoToString(const Value: TValue): string; override;
    procedure CallApi(const Call: TClientVirtualApiCall); override;
  end;
  {$M-}

implementation

procedure TJSONClientVirtualApi<T, IConfiguration>.CallApi(const Call: TClientVirtualApiCall);
var
  ApiClient: IShared<TidHttp>;
  Pair: TPair<string, string>;
  Parameter: TClientVirtualApiCallParameter;
  Index: Integer;
  Query: string;
  FormData: IShared<TStringList>;
  SourceStream: IShared<TStringStream>;
  ResponseContent: string;
  Headers: IShared<TStringList>;
begin
  inherited;

  Guard.CheckNotNull(Call, 'Call');
  Guard.CheckNotNull(Call.ResponseHeaders, 'Call.ResponseHeaders');

  ApiClient := Shared.Make(TIdHttp.Create(nil));
  ApiClient.HandleRedirects := Call.HandleRedirects;
  ApiClient.Request.UserAgent := 'FidoLib websocket client';
  ApiClient.Request.ContentType := GetAcceptHeaderWithApiVersion(Call.ContentType);
  ApiClient.Request.CharSet := 'utf-8';
  ApiClient.Response.ContentType := GetAcceptHeaderWithApiVersion(Call.ContentType);

  ApiClient.ConnectTimeout := Call.Timeout;
  ApiClient.ReadTimeout := Call.Timeout;

  if Call.Url.StartsWith('https://', True) then
  begin
    ApiClient.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(ApiClient);
    TIdSSLIOHandlerSocketOpenSSL(ApiClient.IOHandler).SSLOptions.Mode := TIdSSLMode.sslmClient;
    TIdSSLIOHandlerSocketOpenSSL(ApiClient.IOHandler).SSLOptions.SSLVersions := [TIdSSLVersion.sslvTLSv1, TIdSSLVersion.sslvTLSv1_1, TIdSSLVersion.sslvTLSv1_2];
    TIdSSLIOHandlerSocketOpenSSL(ApiClient.IOHandler).SSLOptions.Method := sslvTLSv1_2;
    TIdSSLIOHandlerSocketOpenSSL(ApiClient.IOHandler).PassThrough := False;
  end;

  FormData := Shared.Make(TStringList.Create);

  for Index := 0 to Call.Parameters.Count - 1 do
  begin
    Parameter := Call.Parameters[Index];
    Guard.CheckFalse(Parameter.Value.IsEmpty);
    case Parameter.Kind of
      pkQuery: begin
        Query := Query + Utilities.IfThen<string>(Query.IsEmpty, '', '&') + Format('%s=%s', [Parameter.Name, Parameter.Value]);
      end;
      pkHeader: begin
        ApiClient.Request.CustomHeaders.AddValue(Parameter.Name, Parameter.Value);
      end;
      pkForm: begin
        FormData.AddPair(Parameter.Name, Parameter.Value);
      end;
      pkFile:;  // Not supported;
    end;
  end;
  if not Query.IsEmpty then
    Query := '?' + Query;

  if not Call.PostBody.IsEmpty then
    SourceStream := Shared.Make(TStringStream.Create(Call.PostBody, TEncoding.UTF8))
  else
    SourceStream := Shared.Make(TStringStream.Create(FormData.DelimitedText, TEncoding.UTF8));

  try
    case Call.ApiMethod of
      rmUnknown: raise EClientVirtualApi.Create(Format('Method %s is not supported.', [SHttpMethod[rmUnknown]]));
      rmGET: ResponseContent := ApiClient.Get(Call.Url + Query);
      rmPOST: ResponseContent := ApiClient.Post(Call.Url + Query, SourceStream);
      rmPUT: ResponseContent := ApiClient.Put(Call.Url + Query, SourceStream);
      rmPATCH: ResponseContent := ApiClient.Patch(Call.Url + Query, SourceStream);
      rmDELETE: ResponseContent := ApiClient.Delete(Call.Url + Query);
      rmHEAD: ApiClient.Head(Call.Url + Query);
      rmOPTIONS: ResponseContent := ApiClient.Options(Call.Url + Query);
      rmCOPY: raise EClientVirtualApi.Create(Format('Method %s is not supported.', [SHttpMethod[rmCOPY]]));
      rmLINK: raise EClientVirtualApi.Create(Format('Method %s is not supported.', [SHttpMethod[rmLINK]]));
      rmUNLINK: raise EClientVirtualApi.Create(Format('Method %s is not supported.', [SHttpMethod[rmUNLINK]]));
      rmPURGE: raise EClientVirtualApi.Create(Format('Method %s is not supported.', [SHttpMethod[rmPURGE]]));
      rmLOCK: raise EClientVirtualApi.Create(Format('Method %s is not supported.', [SHttpMethod[rmLOCK]]));
      rmUNLOCK: raise EClientVirtualApi.Create(Format('Method %s is not supported.', [SHttpMethod[rmUNLOCK]]));
      rmPROPFIND: raise EClientVirtualApi.Create(Format('Method %s is not supported.', [SHttpMethod[rmPROPFIND]]));
      rmVIEW: raise EClientVirtualApi.Create(Format('Method %s is not supported.', [SHttpMethod[rmVIEW]]));
    end;

    Headers := Shared.Make(TStringList.Create);
    for Index := 0 to ApiClient.Response.RawHeaders.Count - 1 do
      Headers.AddPair(ApiClient.Response.RawHeaders.Names[Index], ApiClient.Response.RawHeaders.Values[ApiClient.Response.RawHeaders.Names[Index]]);

    Call.Finish(ApiClient.ResponseCode, ResponseContent, Headers);
  except
    on E: Exception do
    begin
      Headers := Shared.Make(TStringList.Create);
      for Index := 0 to ApiClient.Response.RawHeaders.Count - 1 do
        Headers.AddPair(ApiClient.Response.RawHeaders.Names[Index], ApiClient.Response.RawHeaders.Values[ApiClient.Response.RawHeaders.Names[Index]]);
      Call.Finish(-1, E.Message, Headers);
    end;
  end;
end;

function TJSONClientVirtualApi<T, IConfiguration>.ConvertRawRequestDtoToString(const Value: TValue): string;
begin
  Result := Value.AsString;
end;

function TJSONClientVirtualApi<T, IConfiguration>.ConvertRequestDtoToString(const Value: TValue): string;
begin
  inherited;
  Result := JSONMarshaller.From(Value, Value.TypeInfo);
end;

function TJSONClientVirtualApi<T, IConfiguration>.ConvertResponseToDto(
  const Response: string;
  const TypeInfo: PTypeInfo): TValue;
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
