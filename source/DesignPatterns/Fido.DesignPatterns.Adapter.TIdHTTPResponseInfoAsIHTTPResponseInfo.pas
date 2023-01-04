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

unit Fido.DesignPatterns.Adapter.TIdHTTPResponseInfoAsIHTTPResponseInfo;

interface

uses
  System.Classes,
  System.SysUtils,
  System.NetEncoding,

  IdCustomHTTPServer,
  IdGlobal,
  IdUri,
  idContext,
  IdGlobalProtocols,
  idSSL,
  IdHashSHA,

  Spring,
  Spring.Collections,

  Fido.Utilities,
  Fido.Http.RequestInfo.Intf,
  Fido.Http.ResponseInfo.Intf;

type
  TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator = class(TInterfacedObject, IHTTPResponseInfo)
  private
    FContext: TIdContext;
    FRequestInfo: TIdHTTPRequestInfo;
    FResponseInfo: TIdHTTPResponseInfo;
    FEncoding: IIdTextEncoding;
    FRawHeaders: TStrings;

    function Encoding: IIdTextEncoding;
  public
    constructor Create(const Context: TIdContext; const RequestInfo: TIdHTTPRequestInfo; const ResponseInfo: TIdHTTPResponseInfo); reintroduce;
    destructor Destroy; override;

    procedure SetContentStream(const Stream: TStream);
    procedure SetResponseCode(const ResponseCode: Integer);
    function ResponseCode: Integer;
    procedure SetContentType(const ContentType: string);
    function EncodeString(const AString: string): string;
    procedure SetResponseText(const Text: string);
    procedure SetContentText(const Text: string);
    procedure SetCustomHeaders(const Headers: IDictionary<string, string>);
    function RawHeaders: TStrings;
    procedure WriteBytesToWebSocket(const Buffer: TWSBytes);
    procedure ReadBytesFromWebSocket(var Buffer: TWSBytes; const ByteCount: Integer; const Append: Boolean = True);
    procedure DisconnectWebSocket;
    function GetWebSocketSignature(const Key: string): string;
  end;

implementation

{ TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator }

constructor TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.Create(
  const Context: TIdContext;
  const RequestInfo: TIdHTTPRequestInfo;
  const ResponseInfo: TIdHTTPResponseInfo);
begin
  inherited Create;
  FContext := Utilities.CheckNotNullAndSet(Context, 'Context');
  FRequestInfo := Utilities.CheckNotNullAndSet(RequestInfo, 'RequestInfo');
  FResponseInfo := Utilities.CheckNotNullAndSet(ResponseInfo, 'ResponseInfo');
end;

destructor TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.Destroy;
begin
  FRawHeaders.Free;
  inherited;
end;

procedure TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.DisconnectWebSocket;
begin
  FContext.Connection.Disconnect;
end;

function TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.EncodeString(
  const AString: string): string;
begin
  Result := TIdURI.URLDecode(AString, Encoding);
end;

function TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.Encoding: IIdTextEncoding;
begin
  if Assigned(FEncoding) then
    Exit(FEncoding);

  if FResponseInfo.CharSet <> '' then
    FEncoding := CharsetToEncoding(FResponseInfo.CharSet)
  else
    FEncoding := IndyTextEncoding_UTF8;

  Result := FEncoding;
end;

function TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.GetWebSocketSignature(const Key: string): string;
var
  Hash: IShared<TIdHashSHA1>;
begin
  Hash := Shared.Make(TIdHashSHA1.Create);
  Result := TNetEncoding.Base64.EncodeBytesToString(Hash.HashString(Key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));
end;

function TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.RawHeaders: TStrings;
begin
  if Assigned(FRawHeaders) then
    Exit(FRawHeaders);

  FRawHeaders := TStringList.Create;
  FRawHeaders.BeginUpdate;
  FResponseInfo.RawHeaders.ConvertToStdValues(FRawHeaders);
  FRawHeaders.EndUpdate;
  Result := FRawHeaders;
end;

procedure TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.ReadBytesFromWebSocket(
  var Buffer: TWSBytes;
  const ByteCount: Integer;
  const Append: Boolean);
begin
  FContext.Connection.IOHandler.ReadBytes(TIdBytes(Buffer), ByteCount, Append);
end;

function TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.ResponseCode: Integer;
begin
  Result := FResponseInfo.ResponseNo;
end;

procedure TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.SetContentStream(
  const Stream: TStream);
begin
  FResponseInfo.ContentStream := Stream;
end;

procedure TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.SetContentType(
  const ContentType: string);
begin
  FResponseInfo.ContentType := ContentType;
end;

procedure TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.SetCustomHeaders(
  const Headers: IDictionary<string, string>);
begin
  FResponseInfo.CustomHeaders.Clear;

  with Headers.Keys.GetEnumerator do
    while MoveNext do
      FResponseInfo.CustomHeaders.Values[Current] := Headers.Items[Current];
end;

procedure TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.SetContentText(
  const Text: string);
begin
  FResponseInfo.ContentText := Text;
end;

procedure TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.SetResponseCode(
  const ResponseCode: Integer);
begin
  FResponseInfo.ResponseNo := ResponseCode;
end;

procedure TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.SetResponseText(
  const Text: string);
begin
  FResponseInfo.ResponseText := Text;
end;

procedure TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.WriteBytesToWebSocket(
  const Buffer: TWSBytes);
begin
  FContext.Connection.IOHandler.Write(TIdBytes(Buffer));
end;

end.
