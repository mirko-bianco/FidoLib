(*
 * Copyright 2022 Mirko Bianco (email: writetomirko@gmail.com)
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

unit Fido.DesignPatterns.Adapter.TBrookHTTPRequestAsIHTTPRequest;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Defaults,

  Spring.Collections,

  BrookHTTPRequest,
  BrookStringMap,

  Fido.Utilities,
  Fido.Http.Types,
  Fido.Http.Utils,
  Fido.Http.Request.Intf;

type
  TBrookHTTPRequestAsIHTTPRequestDecorator = class(TInterfacedObject, IHttpRequest)
  private
    FRequest: TBrookHTTPRequest;
    FFormParams: IDictionary<string, string>;
    FQueryParams: IDictionary<string, string>;
    FHeaders: IDictionary<string, string>;

    procedure BrookMapToDictionary(const Map: TBrookStringMap; const Dictionary: IDictionary<string, string>);
  public
    constructor Create(const Request: TBrookHTTPRequest); reintroduce;

    function Method: THttpMethod;
    function URI: string;
    function Body: string;
    function FormParams: IDictionary<string, string>;
    function HeaderParams: IDictionary<string, string>;
    function QueryParams: IDictionary<string, string>;
    function MimeType: TMimeType;
  end;

implementation

{ TBrookHTTPRequestAsIHTTPRequestDecorator }

function TBrookHTTPRequestAsIHTTPRequestDecorator.Body: string;
begin
  Result := FRequest.Payload.ToString;
end;

procedure TBrookHTTPRequestAsIHTTPRequestDecorator.BrookMapToDictionary(const Map: TBrookStringMap; const Dictionary: IDictionary<string, string>);
begin
  with Map.GetEnumerator do
    try
      while MoveNext do
        Dictionary[GetCurrent.Name] := GetCurrent.Value;
    finally
      Free;
    end;
end;

constructor TBrookHTTPRequestAsIHTTPRequestDecorator.Create(const Request: TBrookHTTPRequest);
begin
  inherited Create;

  FRequest := Utilities.CheckNotNullAndSet<TBrookHTTPRequest>(Request, 'Request');

  FFormParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);
  FQueryParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);
  FHeaders := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);

  BrookMapToDictionary(FRequest.Fields, FFormParams);
  BrookMapToDictionary(FRequest.Params, FQueryParams);
  BrookMapToDictionary(FRequest.Headers, FHeaders);
end;

function TBrookHTTPRequestAsIHTTPRequestDecorator.FormParams: IDictionary<string, string>;
begin
  Result := FFormParams;
end;

function TBrookHTTPRequestAsIHTTPRequestDecorator.HeaderParams: IDictionary<string, string>;
begin
  Result := FHeaders;
end;

function TBrookHTTPRequestAsIHTTPRequestDecorator.Method: THttpMethod;
begin
  Result := ConvertToRestCommand(FRequest.Method);
end;

function TBrookHTTPRequestAsIHTTPRequestDecorator.MimeType: TMimeType;
begin
  Result := ContentTypeToMimeType(FRequest.ContentType);
end;

function TBrookHTTPRequestAsIHTTPRequestDecorator.QueryParams: IDictionary<string, string>;
begin
  Result := FQueryParams;
end;

function TBrookHTTPRequestAsIHTTPRequestDecorator.URI: string;
begin
  Result := FRequest.Path;
end;

end.
