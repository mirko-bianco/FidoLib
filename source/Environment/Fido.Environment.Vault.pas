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
  private
    class var Secrets: IDictionary<string, string>;
    class function Key(AEnvironment: string; AKey: string): string;
  private
    FEnvironment: string;
    function TryGetSecret(AKey: string; out Value: string): Boolean;
  public
    class procedure Add(AEnvironment: string; AKey: string; AValue: string);

    constructor Create(AEnvironment: string);

    function GetSecret(AKey: string): string; // IVault
  end;

implementation

class procedure TVault.Add(AEnvironment: string; AKey: string; AValue: string);
begin
  Secrets.AddOrSetValue(Key(AEnvironment, AKey), AValue);
end;

constructor TVault.Create(AEnvironment: string);
begin
  FEnvironment := AEnvironment;
end;

function TVault.GetSecret(AKey: string): string;
begin
  if not TryGetSecret(AKey, Result) then
    Result := '';
end;

class function TVault.Key(AEnvironment: string; AKey: string): string;
begin
  Result := AEnvironment.ToLower + '.' + AKey.ToLower;
end;

function TVault.TryGetSecret(AKey: string; out Value: string): Boolean;
begin
  Result := Assigned(Secrets) and Secrets.TryGetValue(Key(FEnvironment, AKey), Value)
end;

initialization
  TVault.Secrets := TCollections.CreateDictionary<string, string>;
end.

