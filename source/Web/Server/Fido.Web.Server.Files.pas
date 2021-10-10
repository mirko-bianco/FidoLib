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

unit Fido.Web.Server.Files;

interface

uses
  System.Classes,
  System.SysUtils,

  Spring,

  Fido.Exceptions,
  Fido.Web.Server.Interpreter.Intf,
  Fido.Http.Request.Intf,
  Fido.Http.Response.Intf,
  Fido.Web.Server.intf,
  Fido.Web.Server.Abstract;

type
  EFidoWebException = class(EFidoException);

  {$M+}
  TFileWebServer = class(TAbstractWebServer, IWebServer)
  private
    FRootFolder: string;
    FIndexFilePath: string;

    function Load(const FilenamePath: string): TStream;
  public
    constructor Create(const RootFolder: string; const IndexFileName: string; const Interpreters: TArray<IWebServerInterpreter> = []);

    function Process(const RestRequest: IHttpRequest; const RestResponse: IHttpResponse): Boolean; override;
  end;

implementation

constructor TFileWebServer.Create(
  const RootFolder: string;
  const IndexFileName: string;
  const Interpreters: TArray<IWebServerInterpreter>);
begin
  inherited Create(Interpreters);

  if not DirectoryExists(RootFolder) then
    raise EFidoException.CreateFmt('Root folder "%s" does not exists', [RootFolder]);

  FRootFolder := IncludeTrailingPathDelimiter(RootFolder);
  if not FileExists(FRootFolder + IndexFileName) then
    raise EFidoWebException.CreateFmt('Index file "%s" does not exist.', [FRootFolder + IndexFileName]);
  FIndexFilePath := FRootFolder + IndexFileName;
end;

function TFileWebServer.Process(
  const RestRequest: IHttpRequest;
  const RestResponse: IHttpResponse): Boolean;
var
  FilenamePath: string;
begin
  if RestRequest.URI.Equals('/') then
    FilenamePath := FIndexFilePath
  else if FileExists(FRootFolder + RestRequest.URI) then
    FilenamePath := FRootFolder + RestRequest.URI
  else
    Exit(False);

  Result := True;
  ProcessResource(Load(FilenamePath), RestResponse);
end;

function TFileWebServer.Load(const FilenamePath: string): TStream;
begin
  Result := TFileStream.Create(FilenamePath, fmOpenRead);
end;

end.
