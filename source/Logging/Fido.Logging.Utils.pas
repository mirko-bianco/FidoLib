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

unit Fido.Logging.Utils;

interface

uses
  System.SysUtils,

  Spring,
  Spring.Logging,

  Fido.Logging.DeadManSwitch;

type
  Logging = class
    class procedure LogData(const Logger: ILogger; const Value: TValue); overload; static;
    class procedure LogData(const Logger: ILogger; const LogLevel: TLogLevel; const Value: TValue); overload; static;
    class procedure LogData(const Logger: ILogger; const Msg: string; const Value: TValue); overload; static;
    class procedure LogDuration(const Logger: ILogger; const ClassName: string; const Method: string; const Proc: Action); overload; static;
    class function LogDuration<T>(const Logger: ILogger; const ClassName: string; const Method: string; const Action: Func<T>): T; overload; static;
  end;

implementation

{ Logging }

class procedure Logging.LogData(
  const Logger: ILogger;
  const LogLevel: TLogLevel;
  const Value: TValue);
begin
  Logger.LogValue(Loglevel, '', Value);
end;

class procedure Logging.LogData(
  const Logger: ILogger;
  const Value: TValue);
begin
  Logger.LogValue(TLoglevel.Info, '', Value);
end;

class procedure Logging.LogData(
  const Logger: ILogger;
  const Msg: string;
  const Value: TValue);
begin
  Logger.LogValue(TLoglevel.Info, Msg, Value);
end;

class procedure Logging.LogDuration(
  const Logger: ILogger;
  const ClassName: string;
  const Method: string;
  const Proc: Action);
var
  DurationLogger: IShared<TDeadManSwitchDurationLogger>;
begin
  DurationLogger := Shared.Make(TDeadManSwitchDurationLogger.Create(Logger, ClassName, Method));
  try
    Proc();
  except
    on E: Exception do
    begin
      DurationLogger.Cancel;
      Logger.Log(TLogLevel.Error, E.Message, E);
      raise;
    end;
  end;
end;

class function Logging.LogDuration<T>(
  const Logger: ILogger;
  const ClassName: string;
  const Method: string;
  const Action: Func<T>): T;
var
  DurationLogger: IShared<TDeadManSwitchDurationLogger>;
begin
  DurationLogger := Shared.Make(TDeadManSwitchDurationLogger.Create(Logger, ClassName, Method));
  try
    Result := Action();
  except
    on E: Exception do
    begin
      DurationLogger.Cancel;
      Logger.Log(TLogLevel.Error, E.Message, E);
      raise;
    end;
  end;
end;

end.
