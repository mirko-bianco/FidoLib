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

unit Fido.Logging.DeadManSwitch;

interface

uses
  System.Classes,
  System.SysUtils,

  Spring,
  Spring.Logging,
  Spring.Logging.Appenders.Base,

  Fido.Logging.Types,
  Fido.Api.Client.VirtualApi.ElasticSearch.Document.Intf,
  Fido.Api.Client.VirtualApi.Elasticsearch.Document.Dto.Request;

type
  TDurationData = class(TLoggedData)
  private
    FTotalDurationMS: Int64;
    FDurationHours: Int64;
    FDurationMinutes: Int64;
    FDurationSeconds: Int64;
    FDurationMilliSeconds: Int64;
  public
    constructor Create(const &Class: string; const Method: string; const DurationHours: Int64; const DurationMinutes: Int64; const DurationSeconds: Int64; const DurationMilliSeconds: Int64);

    function TotalDurationMS: Int64;
    function DurationHours: Int64;
    function DurationMinutes: Int64;
    function DurationSeconds: Int64;
    function DurationMilliSeconds: Int64;
  end;

  TDeadManSwitchDurationLogger = class
  private
    FLogger: ILogger;
    FClassName: string;
    FMethodName: string;
    FDuration: TTime;
    FIsCancelled: Boolean;
  public
    constructor Create(const Logger: ILogger; const ClassName: string; const MethodName: string);
    destructor Destroy; override;

    procedure Cancel;
  end;

implementation

{ TDeadManSwitchDurationLogger }

procedure TDeadManSwitchDurationLogger.Cancel;
begin
  FIsCancelled := True;
end;

constructor TDeadManSwitchDurationLogger.Create(
  const Logger: ILogger;
  const ClassName: string;
  const MethodName: string);
begin
  inherited Create;
  FLogger := Logger;
  FClassName := ClassName;
  FMethodName := MethodName;
  FDuration := Now;

  FIsCancelled := False;
end;

destructor TDeadManSwitchDurationLogger.Destroy;
var
  H, M, S, MS: Word;
begin
  if not FIsCancelled then
  begin
    FDuration := Now - FDuration;
    DecodeTime(FDuration, H, M, S, MS);
    FLogger.Log(
      TLogEvent.Create(
        TLogLevel.Info,
        TLogEventType.Text,
        Format('"%s" executed in %d hours, %d minutes, %d seconds, %d milliseconds.', [FMethodName, H, M, S, MS]),
        nil,
        TDurationData.Create(FClassName, FMethodName, H, M, S, MS)
      ));
  end;
  inherited;
end;

{ TDurationData }

constructor TDurationData.Create(
  const &Class: string;
  const Method: string;
  const DurationHours: Int64;
  const DurationMinutes: Int64;
  const DurationSeconds: Int64;
  const DurationMilliSeconds: Int64);
begin
  inherited Create('Duration', &Class, Method);
  FDurationHours := DurationHours;
  FDurationMinutes := DurationMinutes;
  FDurationSeconds := DurationSeconds;
  FDurationMilliSeconds := DurationMilliSeconds;
  FTotalDurationMS := (DurationHours * 60 * 60 * 1000) + (DurationMinutes * 60 * 1000) + (DurationSeconds * 1000) + DurationMilliSeconds;
end;

function TDurationData.DurationHours: Int64;
begin
  Result := FDurationHours;
end;

function TDurationData.DurationMilliSeconds: Int64;
begin
  Result := FDurationMilliSeconds;
end;

function TDurationData.DurationMinutes: Int64;
begin
  Result := FDurationMinutes;
end;

function TDurationData.DurationSeconds: Int64;
begin
  Result := FDurationSeconds;
end;

function TDurationData.TotalDurationMS: Int64;
begin
  Result := FTotalDurationMS
end;

end.
