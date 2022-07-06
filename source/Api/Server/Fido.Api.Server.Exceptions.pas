(*
 * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without Apiriction, including without limitation the rights
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

unit Fido.Api.Server.Exceptions;

interface

uses
  Spring,
  Spring.Logging,

  Fido.Logging.Types,
  Fido.Exceptions;

type
  EApiServer400 = class(EFidoException); //Bad request

  EApiServer401 = class(EFidoException); //Unauthorized

  EApiServer403 = class(EFidoException); //Forbidden

  EApiServer404 = class(EFidoException); //Not found

  EApiServer409 = class(EFidoException); //Conflict

  EApiServer500 = class(EFidoException) //Internal server error
    constructor Create(const Msg: string; const Logger: ILogger; const &Class: string; const Method: string);
  end;

  EApiServer503 = class(EFidoException); //Service unavailable

  EApiServer504 = class(EFidoException); //Gateway timeout


implementation

{ EApiServer500 }

constructor EApiServer500.Create(
  const Msg: string;
  const Logger: ILogger;
  const &Class: string;
  const Method: string);
var
  LoggedData: Shared<TLoggedData>;
begin
  inherited Create(Msg);

  LoggedData := TLoggedData.Create('Error', &Class, Method);

  Logger.Log(TLogEvent.Create(
    TLogLevel.Error,
    TLogEventType.Text,
    Msg,
    nil,
    TValue.From<TLoggedData>(LoggedData.Value)));
end;

end.
