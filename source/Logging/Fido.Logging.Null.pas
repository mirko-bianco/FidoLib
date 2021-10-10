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

 unit Fido.Logging.Null;

interface

uses
  System.Rtti,
  System.SysUtils,
  Spring.Logging;

type
  TNullLoggerEx = class(TInterfacedObject, ILogger)
  private class var
    fGlobalInstance: ILogger;
  private
    class constructor Create;

    function GetEnabled: Boolean;
    function GetEventTypes: TLogEventTypes;
    function GetLevels: TLogLevels;
  public
    function IsEnabled(level: TLogLevel; eventTypes: TLogEventTypes): Boolean; inline;
    function IsFatalEnabled: Boolean;
    function IsErrorEnabled: Boolean;
    function IsWarnEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsTextEnabled: Boolean;
    function IsDebugEnabled: Boolean;
    function IsTraceEnabled: Boolean;

    procedure Log(const event: TLogEvent); overload;

    procedure LogValue(const name: string; const value: TValue); overload;
    procedure LogValue(level: TLogLevel; const name: string; const value: TValue); overload;

    procedure Log(const msg: string); overload;
    procedure Log(const msg: string; const e: Exception); overload;
    procedure Log(const fmt: string; const args: array of const); overload;
    procedure Log(const fmt: string; const args: array of const; const e: Exception); overload;

    procedure Log(level: TLogLevel; const msg: string); overload;
    procedure Log(level: TLogLevel; const msg: string; const e: Exception); overload;
    procedure Log(level: TLogLevel; const fmt: string; const args: array of const); overload;
    procedure Log(level: TLogLevel; const fmt: string; const args: array of const; const e: Exception); overload;

    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; const e: Exception); overload;
    procedure Fatal(const fmt: string; const args: array of const); overload;
    procedure Fatal(const fmt: string; const args: array of const; const e: Exception); overload;

    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; const e: Exception); overload;
    procedure Error(const fmt: string; const args: array of const); overload;
    procedure Error(const fmt: string; const args: array of const; const e: Exception); overload;

    procedure Warn(const msg: string); overload;
    procedure Warn(const msg: string; const e: Exception); overload;
    procedure Warn(const fmt: string; const args: array of const); overload;
    procedure Warn(const fmt: string; const args: array of const; const e: Exception); overload;

    procedure Info(const msg: string); overload;
    procedure Info(const msg: string; const e: Exception); overload;
    procedure Info(const fmt: string; const args: array of const); overload;
    procedure Info(const fmt: string; const args: array of const; const e: Exception); overload;

    procedure Text(const msg: string); overload;
    procedure Text(const msg: string; const e: Exception); overload;
    procedure Text(const fmt: string; const args: array of const); overload;
    procedure Text(const fmt: string; const args: array of const; const e: Exception); overload;

    procedure Debug(const msg: string); overload;
    procedure Debug(const msg: string; const e: Exception); overload;
    procedure Debug(const fmt: string; const args: array of const); overload;
    procedure Debug(const fmt: string; const args: array of const; const e: Exception); overload;

    procedure Trace(const msg: string); overload;
    procedure Trace(const msg: string; const e: Exception); overload;
    procedure Trace(const fmt: string; const args: array of const); overload;
    procedure Trace(const fmt: string; const args: array of const; const e: Exception); overload;

    procedure Enter(const methodName: string); overload;
    procedure Enter(const classType: TClass; const methodName: string); overload;
    procedure Enter(const instance: TObject; const methodName: string); overload;
    procedure Enter(level: TLogLevel; const classType: TClass; const methodName: string); overload;

    procedure Leave(const methodName: string); overload;
    procedure Leave(const classType: TClass; const methodName: string); overload;
    procedure Leave(const instance: TObject; const methodName: string); overload;
    procedure Leave(level: TLogLevel; const classType: TClass; const methodName: string); overload;

    function Track(const classType: TClass; const methodName: string): IInterface; overload;
    function Track(const instance: TObject; const methodName: string): IInterface; overload;
    function Track(level: TLogLevel; const classType: TClass; const methodName: string): IInterface; overload;

    class property GlobalInstance: ILogger read fGlobalInstance;
  end;

implementation

{ TNullLoggerEx }

procedure TNullLoggerEx.Log(const event: TLogEvent);
begin
  if Event.Data.IsObject then
    Event.Data.AsObject.Free;
end;

class constructor TNullLoggerEx.Create;
begin
  fGlobalInstance := TNullLoggerEx.Create;
end;

procedure TNullLoggerEx.Log(
  level: TLogLevel;
  const msg: string);
begin
end;

procedure TNullLoggerEx.Log(
  level: TLogLevel;
  const fmt: string;
  const args: array of const);
begin
end;

procedure TNullLoggerEx.Log(
  level: TLogLevel;
  const msg: string;
  const e: Exception);
begin
end;

procedure TNullLoggerEx.Debug(
  const fmt: string;
  const args: array of const);
begin
end;

procedure TNullLoggerEx.Debug(
  const fmt: string;
  const args: array of const;
  const e: Exception);
begin
end;

procedure TNullLoggerEx.Debug(const msg: string);
begin
end;

procedure TNullLoggerEx.Debug(
  const msg: string;
  const e: Exception);
begin
end;

procedure TNullLoggerEx.Error(
  const fmt: string;
  const args: array of const);
begin
end;

procedure TNullLoggerEx.Enter(
  level: TLogLevel;
  const classType: TClass;
  const methodName: string);
begin
end;

procedure TNullLoggerEx.Enter(const methodName: string);
begin
end;

procedure TNullLoggerEx.Enter(
  const classType: TClass;
  const methodName: string);
begin
end;

procedure TNullLoggerEx.Enter(
  const instance: TObject;
  const methodName: string);
begin
end;

procedure TNullLoggerEx.Error(
  const fmt: string;
  const args: array of const;
  const e: Exception);
begin
end;

procedure TNullLoggerEx.Error(const msg: string);
begin
end;

procedure TNullLoggerEx.Error(
 const msg: string;
 const e: Exception);
begin
end;

procedure TNullLoggerEx.Fatal(
  const fmt: string;
  const args: array of const);
begin
end;

procedure TNullLoggerEx.Fatal(
  const fmt: string;
  const args: array of const;
  const e: Exception);
begin
end;

function TNullLoggerEx.GetEnabled: Boolean;
begin
  Result := False;
end;

function TNullLoggerEx.GetEventTypes: TLogEventTypes;
begin
  Result := [];
end;

function TNullLoggerEx.GetLevels: TLogLevels;
begin
  Result := [];
end;

procedure TNullLoggerEx.Fatal(const msg: string);
begin
end;

procedure TNullLoggerEx.Fatal(
  const msg: string;
  const e: Exception);
begin
end;

procedure TNullLoggerEx.Info(
  const msg: string;
  const e: Exception);
begin
end;

procedure TNullLoggerEx.Info(const msg: string);
begin
end;

procedure TNullLoggerEx.Info(
  const fmt: string;
  const args: array of const;
  const e: Exception);
begin
end;

function TNullLoggerEx.IsDebugEnabled: Boolean;
begin
  Result := False;
end;

function TNullLoggerEx.IsEnabled(
  level: TLogLevel;
  eventTypes: TLogEventTypes): Boolean; //FI:O804
begin
  Result := False;
end;

function TNullLoggerEx.IsErrorEnabled: Boolean;
begin
  Result := False;
end;

function TNullLoggerEx.IsFatalEnabled: Boolean;
begin
  Result := False;
end;

function TNullLoggerEx.IsInfoEnabled: Boolean;
begin
  Result := False;
end;

function TNullLoggerEx.IsTextEnabled: Boolean;
begin
  Result := False;
end;

function TNullLoggerEx.IsTraceEnabled: Boolean;
begin
  Result := False;
end;

function TNullLoggerEx.IsWarnEnabled: Boolean;
begin
  Result := False;
end;

procedure TNullLoggerEx.Info(
  const fmt: string;
  const args: array of const);
begin
end;

procedure TNullLoggerEx.Leave(
  level: TLogLevel;
  const classType: TClass;
  const methodName: string);
begin
end;

procedure TNullLoggerEx.Leave(const methodName: string);
begin
end;

procedure TNullLoggerEx.Log(const msg: string);
begin
end;

procedure TNullLoggerEx.Log(
  const msg: string;
  const e: Exception);
begin
end;

procedure TNullLoggerEx.Log(
  const fmt: string;
  const args: array of const);
begin
end;

procedure TNullLoggerEx.Log(
  const fmt: string;
  const args: array of const;
  const e: Exception);
begin
end;

procedure TNullLoggerEx.Leave(
  const instance: TObject;
  const methodName: string);
begin
end;

procedure TNullLoggerEx.Leave(
  const classType: TClass;
  const methodName: string);
begin
end;

procedure TNullLoggerEx.Log(
  level: TLogLevel;
  const fmt: string;
  const args: array of const; const e: Exception);
begin
end;

procedure TNullLoggerEx.LogValue(
  level: TLogLevel;
  const name: string;
  const value: TValue);
begin
  if value.IsObject then
    value.AsObject.Free;
end;

procedure TNullLoggerEx.LogValue(
  const name: string;
  const value: TValue);
begin
  if value.IsObject then
    value.AsObject.Free;
end;

procedure TNullLoggerEx.Text(
  const fmt: string;
  const args: array of const);
begin
end;

procedure TNullLoggerEx.Text(
  const fmt: string;
  const args: array of const;
  const e: Exception);
begin
end;

function TNullLoggerEx.Track(
  const instance: TObject;
  const methodName: string): IInterface; //FI:O804
begin
  Result := nil;
end;

function TNullLoggerEx.Track(
  const classType: TClass;
  const methodName: string): IInterface; //FI:O804
begin
  Result := nil;
end;

function TNullLoggerEx.Track(
  level: TLogLevel;
  const classType: TClass;
  const methodName: string): IInterface; //FI:O804
begin
  Result := nil;
end;

procedure TNullLoggerEx.Text(const msg: string);
begin
end;

procedure TNullLoggerEx.Text(
  const msg: string;
  const e: Exception);
begin
end;

procedure TNullLoggerEx.Trace(
  const fmt: string;
  const args: array of const);
begin
end;

procedure TNullLoggerEx.Trace(
  const fmt: string;
  const args: array of const;
  const e: Exception);
begin
end;

procedure TNullLoggerEx.Trace(const msg: string);
begin
end;

procedure TNullLoggerEx.Trace(
  const msg: string;
  const e: Exception);
begin
end;

procedure TNullLoggerEx.Warn(
  const fmt: string;
  const args: array of const);
begin
end;

procedure TNullLoggerEx.Warn(
  const fmt: string;
  const args: array of const;
  const e: Exception);
begin
end;

procedure TNullLoggerEx.Warn(const msg: string);
begin
end;

procedure TNullLoggerEx.Warn(
  const msg: string;
  const e: Exception);
begin
end;

end.
