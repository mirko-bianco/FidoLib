(*
 * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
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

unit Fido.Http.Request;

interface

uses
  System.Classes,
  System.SysUtils,
  Generics.Defaults,

  Spring,
  Spring.Collections,

  Fido.Http.Types,
  Fido.Http.Utils,
  Fido.Http.RequestInfo.Intf,
  Fido.Http.Request.Intf;

type
  THttpRequest = class(TInterfacedObject, IHttpRequest)
  private
    FMethod: THttpMethod;
    FURI: string;
    FBody: string;
    FFormParams: IDictionary<string, string>;
    FQueryParams: IDictionary<string, string>;
    FHeaderParams: IDictionary<string, string>;
    FMimeType: TMimeType;
  public
    constructor Create(const RequestInfo: IHttpRequestInfo);

    function Method: THttpMethod;
    function URI: string;
    function Body: string;
    function FormParams: IDictionary<string, string>;
    function HeaderParams: IDictionary<string, string>;
    function QueryParams: IDictionary<string, string>;
    function MimeType: TMimeType;
  end;

implementation

{ THttpRequest }

function THttpRequest.URI: string;
begin
  Result := FURI;
end;

constructor THttpRequest.Create(const RequestInfo: IHttpRequestInfo);
var
  TempBodyParams: IShared<TStringList>;
begin
  Guard.CheckNotNull(RequestInfo, 'RequestInfo');

  inherited Create;

  FFormParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);
  FQueryParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);
  FHeaderParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);

  FMimeType := ContentTypeToMimeType(RequestInfo.ContentType.ToUpper);

  TempBodyParams := Shared.Make(TStringList.Create);

  if Assigned(RequestInfo.PostStream) then
    TempBodyParams.LoadFromStream(RequestInfo.PostStream);

  FBody := RequestInfo.UnparsedParams;
  if FBody.IsEmpty then
    FBody := TempBodyParams.Text;

  StringsToDictionary(RequestInfo.FormParams, FFormParams);
  StringsToDictionary(RequestInfo.QueryParams, FQueryParams);
  StringsToDictionary(RequestInfo.RawHeaders, FHeaderParams);

  FUri := RequestInfo.URI;

  FMethod := ConvertToRestCommand(RequestInfo.Command);
end;

function THttpRequest.Method: THttpMethod;
begin
  Result := FMethod;
end;

function THttpRequest.MimeType: TMimeType;
begin
  Result := FMimeType;
end;

function THttpRequest.Body: string;
begin
  Result := FBody;
end;

function THttpRequest.FormParams: IDictionary<string, string>;
begin
  Result := FFormParams;
end;

function THttpRequest.HeaderParams: IDictionary<string, string>;
begin
  Result := FHeaderParams;
end;

function THttpRequest.QueryParams: IDictionary<string, string>;
begin
  Result := FQueryParams;
end;

end.
