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

unit Fido.Environment.Vault;

interface

uses
  System.Generics.Collections,
  SysUtils,

  Spring.Collections,

  Fido.Environment.Vault.Intf;

type
  TVault = class(TInterfacedObject, IVault)
  private class var
    Secrets: IDictionary<string, string>;
  private
    FEnvironment: string;

    class function CalculateKey(const Environment: string; const Key: string): string;
    function TryGetSecret(const Key: string; out Value: string): Boolean;
  public
    constructor Create(const Environment: string);

    class procedure Add(const Environment: string; const Key: string; const Value: string);
    // IVault
    function GetSecret(const Key: string): string;
  end;

implementation

class procedure TVault.Add(
  const Environment: string;
  const Key: string;
  const Value: string);
begin
  Secrets.AddOrSetValue(CalculateKey(Environment, Key), Value);
end;

constructor TVault.Create(const Environment: string);
begin
  FEnvironment := Environment;
end;

function TVault.GetSecret(const Key: string): string;
begin
  if not TryGetSecret(Key, Result) then
    Result := '';
end;

class function TVault.CalculateKey(
  const Environment: string;
  const Key: string): string;
begin
  Result := Environment.ToLower + '.' + Key.ToLower;
end;

function TVault.TryGetSecret(
  const Key: string;
  out Value: string): Boolean;
begin
  Result := Assigned(Secrets) and Secrets.TryGetValue(CalculateKey(FEnvironment, Key), Value)
end;

initialization
  TVault.Secrets := TCollections.CreateDictionary<string, string>;
end.

