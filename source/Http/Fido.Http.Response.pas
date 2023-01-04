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

unit Fido.Http.Response;

interface

uses
  System.Classes,
  System.SysUtils,
  Generics.Defaults,

  Spring,
  Spring.Collections,

  Fido.Utilities,
  Fido.Http.Types,
  Fido.Http.RequestInfo.Intf,
  Fido.Http.ResponseInfo.Intf,
  Fido.Http.Response.Intf,
  Fido.DesignPatterns.Adapter.TIdHTTPRequestInfoAsIHTTPRequestInfo;

type
  THttpResponse = class(TInterfacedObject, IHttpResponse)
  private
    FRequestInfo: IHTTPRequestInfo;
    FResponseInfo: IHTTPResponseInfo;
    FResponseCode: Integer;
    FBody: string;
    FHeaderParams: IDictionary<string, string>;
    FMimeType: TMimeType;
    FResponseText: string;

    procedure StringsToDictionary(const Strings: TStrings; const Dictionary: IDictionary<string, string>);

    procedure ApplyChanges;
  public
    constructor Create(
      const RequestInfo: IHTTPRequestInfo;
      const ResponseInfo: IHTTPResponseInfo);

    procedure SetResponseCode(const ResponseCode: Integer;const ResponseText: string = '');
    function Body: string;
    procedure SetBody(const Body: string);
    procedure SetStream(const Stream: TStream);
    function HeaderParams: IDictionary<string, string>;
    function MimeType: TMimeType;
    procedure SetMimeType(const MimeType: TMimeType);
  end;

implementation

{ THttpResponse }

procedure THttpResponse.SetBody(const Body: string);
begin
  FBody := Body;
end;

procedure THttpResponse.SetMimeType(const MimeType: TMimeType);
begin
  FMimeType := MimeType;
  FResponseInfo.SetContentType(SMimeType[MimeType]);
end;

procedure THttpResponse.SetResponseCode(
  const ResponseCode: Integer;
  const ResponseText: string);
begin
  FResponseCode := ResponseCode;
  FResponseText := ResponseText;
  ApplyChanges;
end;

procedure THttpResponse.SetStream(const Stream: TStream);
begin
  FResponseInfo.SetContentStream(Stream);
end;

procedure THttpResponse.StringsToDictionary(
  const Strings: TStrings;
  const Dictionary: IDictionary<string, string>);
var
  I: Integer;
begin
  Guard.CheckNotNull(Strings, 'Strings');
  Guard.CheckNotNull(Dictionary, 'Dictionary');

  Strings.Text := FResponseInfo.EncodeString(Strings.Text);

  for I := 0 to Strings.Count - 1 do
    Dictionary[Strings.Names[I]] := Strings.ValueFromIndex[I];
end;

constructor THttpResponse.Create(
  const RequestInfo: IHTTPRequestInfo;
  const ResponseInfo: IHTTPResponseInfo);
var
  TempBodyParams: IShared<TStringList>;
  Accepts: IShared<TStringList>;
  MimeLine: IShared<TStringList>;
  MimeTypeIndex: Integer;
  StringMimeType: string;
  I: Integer;
  Found: Boolean;
begin
  FRequestInfo := Utilities.CheckNotNullAndSet(RequestInfo, 'RequestInfo');
  FResponseInfo := Utilities.CheckNotNullAndSet(ResponseInfo, 'ResponseInfo');

  inherited Create;

  FHeaderParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);

  TempBodyParams := Shared.Make(TStringList.Create);

  Accepts := Shared.Make(TStringList.Create);
  Accepts.Delimiter := ',';
  Accepts.DelimitedText := RequestInfo.Accept;

  Found := False;
  FMimeType := mtDefault;
  for I := 0 to Accepts.Count - 1 do
  begin
    MimeLine := Shared.Make(TStringList.Create);
    MimeLine.Delimiter := ';';
    MimeLine.DelimitedText := Accepts[I];

    MimeTypeIndex := -1;
    for StringMimeType in SMimeType do
    begin
      Inc(MimeTypeIndex);
      if Trim(MimeLine[0].ToUpper) = StringMimeType.ToUpper then
      begin
        Found := True;
        FMimeType := TMimeType(MimeTypeIndex);
        Break;
      end;
    end;

    if Found then
      Break
  end;

  StringsToDictionary(RequestInfo.RawHeaders, FHeaderParams);

  FHeaderParams.Remove('Content-Length');
  FHeaderParams.Remove('Content-type');

  FResponseCode := ResponseInfo.ResponseCode;
  ResponseInfo.SetContentType(SMimeType[FMimeType]);
end;

procedure THttpResponse.ApplyChanges;
begin
  FResponseInfo.SetResponseCode(FResponseCode);
  if not FResponseText.IsEmpty then
    FResponseInfo.SetResponseText(FResponseText);
  FResponseInfo.SetContentText(Body);

  FResponseInfo.SetCustomHeaders(FHeaderParams);
end;

function THttpResponse.Body: string;
begin
  Result := FBody;
end;

function THttpResponse.HeaderParams: IDictionary<string, string>;
begin
  Result := FHeaderParams;
end;

function THttpResponse.MimeType: TMimeType;
begin
  Result := FMimeType;
end;

end.
