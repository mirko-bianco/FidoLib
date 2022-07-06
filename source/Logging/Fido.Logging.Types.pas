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

unit Fido.Logging.Types;

interface

uses
  System.SysUtils;

type
  {$M+}
  TLoggedData = class
  private
    FLogType: string;
    FClass: string;
    FMethod: string;

  public
    constructor Create(const LogType: string; const &Class: string; const Method: string);

    function ToString: string; override;
  published
    function LogType: string;
    function &Class: string;
    function Method: string;
  end;
  {$M-}

implementation

{ TLoggedData }

function TLoggedData.&Class: string;
begin
  Result := FClass;
end;

constructor TLoggedData.Create(
  const LogType: string;
  const &Class: string;
  const Method: string);
begin
  inherited Create;
  FLogType := LogType;
  FClass := &Class;
  FMethod := Method;
end;

function TLoggedData.LogType: string;
begin
  Result := FLogType;
end;

function TLoggedData.Method: string;
begin
  Result := FMethod
end;

function TLoggedData.ToString: string;
begin
  Result := Format('%s. %s.%s', [FLogType, FClass, FMethod]);
end;

end.
