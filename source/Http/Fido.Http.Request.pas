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

    constructor StringsToDictionary(const Strings: TStrings; const Dictionary: IDictionary<string, string>);
  public
    constructor Create(const RequestInfo: IHttpRequestInfo);

    function Method: THttpMethod;
    function URI: string;
    function Body: string;
    function FormParams: IReadOnlyDictionary<string, string>;
    function HeaderParams: IReadOnlyDictionary<string, string>;
    function QueryParams: IReadOnlyDictionary<string, string>;
    function MimeType: TMimeType;
  end;

implementation

{ THttpRequest }

constructor THttpRequest.StringsToDictionary(
  const Strings: TStrings;
  const Dictionary: IDictionary<string, string>);
var
  I: Integer;
begin
  Guard.CheckNotNull(Strings, 'Strings');
  Guard.CheckNotNull(Dictionary, 'Dictionary');

  for I := 0 to Strings.Count - 1 do
    Dictionary.AddOrSetValue(Strings.Names[I], Strings.ValueFromIndex[I]);
end;

function THttpRequest.URI: string;
begin
  Result := FURI;
end;

constructor THttpRequest.Create(const RequestInfo: IHttpRequestInfo);
var
  TempBodyParams: Shared<TStringList>;
  StringMimeType: string;
  MimeTypeIndex: Integer;
  ContentType: string;

  function ConvertToRestCommand(const Item: string): THttpMethod;
  var
    I: Integer;
  begin
    Result := rmUnknown;

    for I := 0 to Integer(High(SHttpMethod)) do
      if UpperCase(SHttpMethod[THttpMethod(I)]) = UpperCase(Item) then
        Exit(THttpMethod(I));
  end;
begin
  Guard.CheckNotNull(RequestInfo, 'RequestInfo');

  inherited Create;

  FFormParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);
  FQueryParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);
  FHeaderParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);

  MimeTypeIndex := -1;
  ContentType := RequestInfo.ContentType.ToUpper;
  if ContentType.IsEmpty then
    ContentType := DEFAULTMIME;
  for StringMimeType in SMimeType do
  begin
    Inc(MimeTypeIndex);
    if ContentType.ToUpper = StringMimeType.ToUpper then
      Break;
  end;
  FMimeType := TMimeType(MimeTypeIndex);

  TempBodyParams := TStringList.Create;

  if Assigned(RequestInfo.PostStream) then
    TempBodyParams.Value.LoadFromStream(RequestInfo.PostStream);

  FBody := RequestInfo.UnparsedParams;
  if FBody.IsEmpty then
    FBody := TempBodyParams.Value.Text;

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

function THttpRequest.FormParams: IReadOnlyDictionary<string, string>;
begin
  Result := FFormParams.AsReadOnlyDictionary;
end;

function THttpRequest.HeaderParams: IReadOnlyDictionary<string, string>;
begin
  Result := FHeaderParams.AsReadOnlyDictionary;
end;

function THttpRequest.QueryParams: IReadOnlyDictionary<string, string>;
begin
  Result := FQueryParams.AsReadOnlyDictionary;
end;

end.
