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

unit Fido.Web.Server.Abstract;

interface

uses
  System.Classes,

  Spring,

  Fido.Web.Server.Interpreter.Intf,
  Fido.Http.Request.Intf,
  Fido.Http.Response.Intf,
  Fido.Web.Server.intf;

type
  TAbstractWebServer = class(TInterfacedObject, IWebServer)
  protected
    FInterpreters: TArray<IWebServerInterpreter>;

    procedure ProcessResource(const SourceStream: TStream; const RestResponse: IHttpResponse);
    function Interpret(const SourceStream: TStream): TStream;
  public
    constructor Create(const Interpreters: TArray<IWebServerInterpreter> = []);

    function Process(const RestRequest: IHttpRequest; const RestResponse: IHttpResponse): Boolean; virtual; abstract;
  end;

implementation

procedure TAbstractWebServer.ProcessResource(
  const SourceStream: TStream;
  const RestResponse: IHttpResponse);
var
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    TempStream.CopyFrom(SourceStream, SourceStream.Size);
  finally
    SourceStream.Free;
  end;
  RestResponse.SetStream(Interpret(TempStream));
end;

constructor TAbstractWebServer.Create(const Interpreters: TArray<IWebServerInterpreter>);
begin
  inherited Create;
  FInterpreters := Interpreters;
end;

function TAbstractWebServer.Interpret(const SourceStream: TStream): TStream;
var
  Interpreter: IWebServerInterpreter;
  DestinationStream: TStream;
begin
  Result := SourceStream;
  for Interpreter in FInterpreters do
  begin
    Result.Position := 0;
    Interpreter.Interpret(Result, DestinationStream);
    Result.Free;
    Result := DestinationStream;
  end;
end;

end.
