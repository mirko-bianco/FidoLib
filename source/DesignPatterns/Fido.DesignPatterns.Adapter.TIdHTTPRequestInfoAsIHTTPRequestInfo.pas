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

unit Fido.DesignPatterns.Adapter.TIdHTTPRequestInfoAsIHTTPRequestInfo;

interface

uses
  System.Classes,
  System.SysUtils,
  IdCustomHTTPServer,
  IdGlobal,
  IdUri,
  IdGlobalProtocols,

  Fido.Http.RequestInfo.Intf;

type
  TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator = class(TInterfacedObject, IHttpRequestInfo)
  private
    FRequestInfo: TIdHTTPRequestInfo;
    FEncoding: IIdTextEncoding;
    FFormParams: TStrings;
    FQueryParams: TStrings;
    FRawHeaders: TStrings;

    procedure ParseParams(const Encoding: IIdTextEncoding; const Params: TStrings; const Value: string);
    function GetEncoding: IIdTextEncoding;
  public
    constructor Create(const RequestInfo: TIdHTTPRequestInfo); reintroduce;
    destructor Destroy; override;

    function ContentType: string;
    function FormParams: TStrings;
    function QueryParams: TStrings;
    function RawHeaders: TStrings;
    function PostStream: TStream;
    function UnparsedParams: string;
    function URI: string;
    function Command: string;
    function Accept: string;
  end;

implementation

{ TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator }

function TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.Accept: string;
begin
  Result := FRequestInfo.Accept;
end;

function TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.Command: string;
begin
  Result := FRequestInfo.Command;
end;

function TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.ContentType: string;
begin
  Result := FRequestInfo.ContentType;
end;

constructor TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.Create(const RequestInfo: TIdHTTPRequestInfo);
begin
  inherited Create;
  FRequestInfo := RequestInfo;
end;

destructor TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.Destroy;
begin
  FFormParams.Free;
  FQueryParams.Free;
  FRawHeaders.Free;
  inherited;
end;

function TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.FormParams: TStrings;
begin
  if Assigned(FFormParams) then
    Exit(FFormParams);

  FFormParams := TStringList.Create;
  ParseParams(GetEncoding, FFormParams, FRequestInfo.FormParams);

  Result := FFormParams;
end;

function TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.GetEncoding: IIdTextEncoding;
begin
  if not Assigned(FEncoding) then
    if FRequestInfo.CharSet <> '' then
      FEncoding := CharsetToEncoding(FRequestInfo.CharSet)
    else
      FEncoding := IndyTextEncoding_UTF8;
  Result := FEncoding;
end;

procedure TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.ParseParams(
  const Encoding: IIdTextEncoding;
  const Params: TStrings;
  const Value: string);
var
  I, J : Integer;
  Token: string;
begin
  Params.BeginUpdate;
  try
    Params.Clear;
    I := 1;
    while I <= Length(Value) do
    begin
      J := I;
      while (J <= Length(Value)) and (Value[J] <> '&') do {do not localize}
        Inc(J);
      Token := Copy(Value, I, J-I);
      Token := StringReplace(Token, '+', ' ', [rfReplaceAll]);
      Params.Add(TIdURI.URLDecode(Token, Encoding));
      I := J + 1;
    end;
  finally
    Params.EndUpdate;
  end;
end;

function TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.PostStream: TStream;
begin
  Result := FRequestInfo.PostStream;
end;

function TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.QueryParams: TStrings;
begin
  if Assigned(FQueryParams) then
    Exit(FQueryParams);

  FQueryParams := TStringList.Create;
  ParseParams(GetEncoding, FQueryParams, FRequestInfo.QueryParams);

  Result := FQueryParams;
end;

function TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.RawHeaders: TStrings;
begin
  if Assigned(FRawHeaders) then
    Exit(FRawHeaders);

  FRawHeaders := TStringList.Create;
  FRawHeaders.BeginUpdate;
  FRequestInfo.RawHeaders.ConvertToStdValues(FRawHeaders);
  FRawHeaders.EndUpdate;
  Result := FRawHeaders;
end;

function TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.UnparsedParams: string;
begin
  Result := TIdURI.URLDecode(FRequestInfo.UnparsedParams, FEncoding);
end;

function TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.URI: string;
begin
  Result := FRequestInfo.URI;
end;

end.
