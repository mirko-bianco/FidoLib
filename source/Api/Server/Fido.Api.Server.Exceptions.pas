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
  {$M+}
  EApiServer = class(EFidoException)
  private
    FCode: Integer;
    FShortMsg: string;
  public
    constructor Create(const Code: Integer; const ShortMsg: string; const Msg: string); overload;
    constructor Create(const Code: Integer; const ShortMsg: string; const Msg: string; const Logger: ILogger; const &Class: string; const Method: string); overload;
  published
    function Code: Integer;
    function ShortMsg: string;
  end;
  {$M-}

  EApiServer400 = class(EApiServer)
    constructor Create(const Msg: string);
  end; //Bad request

  EApiServer401 = class(EApiServer)
    constructor Create(const Msg: string);
  end; //Unauthorized

  EApiServer403 = class(EApiServer)
    constructor Create(const Msg: string);
  end; //Forbidden

  EApiServer404 = class(EApiServer)
    constructor Create(const Msg: string);
  end; //Not found

  EApiServer409 = class(EApiServer)
    constructor Create(const Msg: string);
  end; //Conflict

  EApiServer500 = class(EApiServer)
  public
    constructor Create(const Msg: string); overload;
    constructor Create(const Msg: string; const Logger: ILogger; const &Class: string; const Method: string); overload;
  end; //Internal server error

  EApiServer503 = class(EApiServer)
    constructor Create(const Msg: string);
  end; //Service unavailable

  EApiServer504 = class(EApiServer)
    constructor Create(const Msg: string);
  end; //Gateway timeout


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
  Create(Msg);

  LoggedData := TLoggedData.Create('Error', &Class, Method);

  Logger.Log(TLogEvent.Create(
    TLogLevel.Error,
    TLogEventType.Text,
    Msg,
    nil,
    TValue.From<TLoggedData>(LoggedData.Value)));
end;

constructor EApiServer500.Create(const Msg: string);
begin
  inherited Create(500, 'Internal server error', Msg);
end;

{ EApiServer }

function EApiServer.Code: Integer;
begin
  Result := FCode;
end;

constructor EApiServer.Create(const Code: Integer; const ShortMsg: string; const Msg: string);
begin
  inherited Create(Msg);
  FCode := Code;
  FShortMsg := ShortMsg;
end;

constructor EApiServer.Create(const Code: Integer; const ShortMsg: string; const Msg: string; const Logger: ILogger; const &Class, Method: string);
var
  LoggedData: Shared<TLoggedData>;
begin
  Create(Code, ShortMsg, Msg);

  LoggedData := TLoggedData.Create('Error', &Class, Method);

  Logger.Log(TLogEvent.Create(
    TLogLevel.Error,
    TLogEventType.Text,
    Msg,
    nil,
    TValue.From<TLoggedData>(LoggedData.Value)));
end;

function EApiServer.ShortMsg: string;
begin
  Result := FShortMsg;
end;

{ EApiServer400 }

constructor EApiServer400.Create(const Msg: string);
begin
  inherited Create(400, 'Bad request', Msg);
end;

{ EApiServer401 }

constructor EApiServer401.Create(const Msg: string);
begin
  inherited Create(401, 'Unauthorized', Msg);
end;

{ EApiServer403 }

constructor EApiServer403.Create(const Msg: string);
begin
  inherited Create(403, 'Forbidden', Msg);
end;

{ EApiServer404 }

constructor EApiServer404.Create(const Msg: string);
begin
  inherited Create(404, 'Not found', Msg);
end;

{ EApiServer409 }

constructor EApiServer409.Create(const Msg: string);
begin
  inherited Create(409, 'Conflict', Msg);
end;

{ EApiServer503 }

constructor EApiServer503.Create(const Msg: string);
begin
  inherited Create(503, 'Service unavailable', Msg);
end;

{ EApiServer504 }

constructor EApiServer504.Create(const Msg: string);
begin
  inherited Create(504, 'Gateway timeout', Msg);
end;

end.
