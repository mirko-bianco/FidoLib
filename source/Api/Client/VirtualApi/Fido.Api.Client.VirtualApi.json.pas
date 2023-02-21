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
  System.Net.UrlClient,
  System.Net.HttpClient,
  System.NetConsts,

  Spring,
  Spring.Collections,

  Fido.Utilities,
  Fido.Http.Types,
  Fido.Api.Client.VirtualApi.Abstract,
  Fido.Api.Client.VirtualApi.Intf,
  Fido.Json.Marshalling,
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
  ApiClient: IShared<THTTPClient>;
  Parameter: TClientVirtualApiCallParameter;
  Index: Integer;
  Query: string;
  FormData: IShared<TStringList>;
  SourceStream: IShared<TStringStream>;
  ResponseContent: string;
  Headers: IShared<TStringList>;
  Response: IHttpResponse;
begin
  inherited;

  Guard.CheckNotNull(Call, 'Call');
  Guard.CheckNotNull(Call.ResponseHeaders, 'Call.ResponseHeaders');

  if Call.Url.StartsWith('https://', True) then
    ApiClient := Shared.Make(THttpClient(TURLSchemes.GetURLClientInstance('HTTPS')))
  else
    ApiClient := Shared.Make(THttpClient(TURLSchemes.GetURLClientInstance('HTTP')));
  ApiClient.HandleRedirects := Call.HandleRedirects;
  ApiClient.UserAgent := 'FidoLib Http client';
  ApiClient.ContentType := GetAcceptHeaderWithApiVersion(Call.ContentType);
  ApiClient.AcceptCharSet := 'utf-8';

  ApiClient.ConnectionTimeout := Call.Timeout;
  ApiClient.SendTimeout := Call.Timeout;
  ApiClient.ResponseTimeout := Call.Timeout;

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
        ApiClient.CustomHeaders[Parameter.Name] := Parameter.Value;
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
      rmGET: begin
        Response := ApiClient.Get(Call.Url + Query);
        ResponseContent := Response.ContentAsString;
      end;
      rmPOST: begin
        Response := ApiClient.Post(Call.Url + Query, SourceStream);
        ResponseContent := Response.ContentAsString;
      end;
      rmPUT: begin
        Response := ApiClient.Put(Call.Url + Query, SourceStream);
        ResponseContent := Response.ContentAsString;
      end;
      rmPATCH: begin
        Response := ApiClient.Patch(Call.Url + Query, SourceStream);
        ResponseContent := Response.ContentAsString;
      end;
      rmDELETE: begin
        Response := ApiClient.Delete(Call.Url + Query);
        ResponseContent := Response.ContentAsString;
      end;
      rmHEAD: begin
        Response := ApiClient.Head(Call.Url + Query);
        ResponseContent := Response.ContentAsString;
      end;
      rmOPTIONS: begin
        Response := ApiClient.Options(Call.Url + Query);
        ResponseContent := Response.ContentAsString;
      end;
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
    for var Pair in Response.Headers do
      Headers.AddPair(Pair.Name, Pair.Value);

    Call.Finish(Response.StatusCode, ResponseContent, Headers);
  except
    on E: Exception do
    begin
      Headers := Shared.Make(TStringList.Create);
      for var Pair in Response.Headers do
        Headers.AddPair(Pair.Name, Pair.Value);
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
