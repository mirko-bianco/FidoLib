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

unit Fido.Web.Server.Resources;

interface

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,

  Fido.Utilities,
  Fido.Resource.StreamReader.Intf,
  Fido.Web.Server.Interpreter.Intf,
  Fido.Http.Request.Intf,
  Fido.Http.Response.Intf,
  Fido.Web.Server.intf,
  Fido.Web.Server.Abstract;

type
  TResourceWebServer = class(TAbstractWebServer, IWebServer)
  private var
    FStreamResourceReader: IStreamResourceReader;
  private
    function Load(const ResoourceName: string): TStream;
  public
    constructor Create(const StreamResourceReader: IStreamResourceReader; const Interpreters: TArray<IWebServerInterpreter> = []);

    function Process(const RestRequest: IHttpRequest; const RestResponse: IHttpResponse): Boolean; override;
  end;

implementation

constructor TResourceWebServer.Create(
  const StreamResourceReader: IStreamResourceReader;
  const Interpreters: TArray<IWebServerInterpreter>);
begin
  inherited Create(Interpreters);

  FStreamResourceReader := Utilities.CheckNotNullAndSet(StreamResourceReader, 'StreamResourceReader');
end;

function TResourceWebServer.Process(
  const RestRequest: IHttpRequest;
  const RestResponse: IHttpResponse): Boolean;
var
  SourceStream: TStream;
begin
  Result := False;
  try
    SourceStream := Load(StringReplace(StringReplace(RestRequest.URI, '/', '_', [rfReplaceAll]), '.', '_', [rfReplaceAll]));
    Result := Assigned(SourceStream);
    if Result then
      ProcessResource(SourceStream, RestResponse);
  except
  end;
end;

function TResourceWebServer.Load(const ResoourceName: string): TStream;
begin
  Result := FStreamResourceReader.GetResourceStream(ResoourceName);
end;

end.
