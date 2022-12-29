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

unit Fido.DesignPatterns.Adapter.TBrookHTTPResponseAsIHTTPResponse;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Defaults,

  Spring.Collections,

  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookStringMap,

  Fido.Utilities,
  Fido.Http.Types,
  Fido.Http.Utils,
  Fido.Http.Response.Intf;

type
  TBrookHTTPResponseAsIHTTPResponseDecorator = class(TInterfacedObject, IHttpResponse)
  private
    FResponse: TBrookHTTPResponse;
    FMimeType: TMimeType;
    FBodyStream: TStringStream;
    FOwnStream: Boolean;
    FHeaders: IDictionary<string, string>;

    procedure BrookMapToDictionary(const Map: TBrookStringMap; const Dictionary: IDictionary<string, string>);
  public
    constructor Create(const Response: TBrookHTTPResponse; const MimeType: TMimeType); reintroduce;
    destructor Destroy; override;

    procedure SetResponseCode(const ResponseCode: Integer; const ResponseText: string = '');
    function Body: string;
    procedure SetBody(const Body: string);
    procedure SetStream(const Stream: TStream);
    function HeaderParams: IDictionary<string, string>;
    function MimeType: TMimeType;
    procedure SetMimeType(const MimeType: TMimeType);
  end;

implementation

{ TBrookHTTPResponseAsIHTTPResponseDecorator }

function TBrookHTTPResponseAsIHTTPResponseDecorator.Body: string;
begin
  FBodyStream.ReadData<string>(Result, FBodyStream.Size);
end;

procedure TBrookHTTPResponseAsIHTTPResponseDecorator.BrookMapToDictionary(
  const Map: TBrookStringMap;
  const Dictionary: IDictionary<string, string>);
begin
  with Map.GetEnumerator do
    try
      while MoveNext do
        Dictionary[GetCurrent.Name] := GetCurrent.Value;
    finally
      Free;
    end;
end;

constructor TBrookHTTPResponseAsIHTTPResponseDecorator.Create(
  const Response: TBrookHTTPResponse;
  const MimeType: TMimeType);
begin
  inherited Create;

  FResponse := Utilities.CheckNotNullAndSet<TBrookHTTPResponse>(Response, 'Response');
  FMimeType := MimeType;
  FBodyStream := TStringStream.Create('');
  FOwnStream := True;
  FHeaders := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);

  BrookMapToDictionary(FResponse.Headers, FHeaders);
end;

destructor TBrookHTTPResponseAsIHTTPResponseDecorator.Destroy;
begin
  if FOwnStream then
    FBodyStream.Free;
  inherited;
end;

function TBrookHTTPResponseAsIHTTPResponseDecorator.HeaderParams: IDictionary<string, string>;
begin
  Result := FHeaders;
end;

function TBrookHTTPResponseAsIHTTPResponseDecorator.MimeType: TMimeType;
begin
  Result := FMimeType;
end;

procedure TBrookHTTPResponseAsIHTTPResponseDecorator.SetBody(const Body: string);
begin
  FBodyStream.Position := 0;
  FBodyStream.SetSize(Longint(0));
  FBodyStream.WriteString(Body);
end;

procedure TBrookHTTPResponseAsIHTTPResponseDecorator.SetMimeType(const MimeType: TMimeType);
begin
  FMimeType := MimeType;
end;

procedure TBrookHTTPResponseAsIHTTPResponseDecorator.SetResponseCode(const ResponseCode: Integer; const ResponseText: string);
begin
  FHeaders.ForEach(procedure(const Item: TPair<string, string>)
    begin
      FResponse.Headers.AddOrSet(Item.Key, Item.Value);
    end);

  FResponse.Send(FBodyStream.DataString, SMimeType[FMimeType], ResponseCode);
end;

procedure TBrookHTTPResponseAsIHTTPResponseDecorator.SetStream(const Stream: TStream);
begin
  if FOwnStream then
    FBodyStream.Free;
  FOwnStream := False;
  FBodyStream.Position := 0;
  FBodyStream.SetSize(Longint(0));
  FBodyStream.LoadFromStream(Stream);
end;

end.
