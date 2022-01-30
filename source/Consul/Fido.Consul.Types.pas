(*
 * Copyright 2022 Mirko Bianco (email: writetomirko@gmail.com)
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

unit Fido.Consul.Types;

interface

uses
  System.SysUtils;

type
  TConsulHealthCheck = record
  private
    FDeregisterCriticalServiceAfterMinutes: Integer;
    FHTTP: string;
    FIntervalInSeconds: Integer;
    FTimeoutInSeconds: Integer;
  public
    constructor Create(const HTTP: string; const DeregisterCriticalServiceAfterMinutes: Integer = 90; const IntervalInSeconds: Integer = 10; const TimeoutInSeconds: Integer = 1);

    function DeregisterCriticalServiceAfter: string;
    function HTTP: string;
    function Interval: string;
    function Timeout: string;
  end;

implementation

{ TConsulCheck }

constructor TConsulHealthCheck.Create(
  const HTTP: string;
  const DeregisterCriticalServiceAfterMinutes: Integer;
  const IntervalInSeconds: Integer;
  const TimeoutInSeconds: Integer);
begin
  FDeregisterCriticalServiceAfterMinutes := DeregisterCriticalServiceAfterMinutes;
  FHTTP := HTTP;
  FIntervalInSeconds := IntervalInSeconds;
  FTimeoutInSeconds := TimeoutInSeconds;
end;

function TConsulHealthCheck.DeregisterCriticalServiceAfter: string;
begin
  Result := Format('%dm', [FDeregisterCriticalServiceAfterMinutes]);
end;

function TConsulHealthCheck.HTTP: string;
begin
  Result := FHTTP;
end;

function TConsulHealthCheck.Interval: string;
begin
  Result := Format('%ds', [FIntervalInSeconds]);
end;

function TConsulHealthCheck.Timeout: string;
begin
  Result := Format('%ds', [FTimeoutInSeconds]);
end;

end.
