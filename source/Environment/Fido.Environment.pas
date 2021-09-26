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

unit Fido.Environment;

interface

uses
  SysUtils,
  Fido.Environment.Intf,
  Fido.Environment.Vault,
  Fido.Environment.Vault.Intf;

const
  EnvironmentNames: array[TEnvironmentType] of string = ('Development', 'Testing', 'Acceptance', 'Production');

type
  TEnvironment = class(TInterfacedObject, IEnvironment)
  private
    FEnvironmentName: string;
    FEnvironmentType: TEnvironmentType;
    FVault: IVault;
  protected

  public
    constructor Create(Environment: string);

    function GetSecret(Secret: string): string;
    function EnvironmentName: string;
    function EnvironmentType: TEnvironmentType;
  end;

implementation

constructor TEnvironment.Create(Environment: string);
var
  t: TEnvironmentType;
begin
  FVault := TVault.Create(Environment);

  // Get type based on name. In case of no match, assume development.
  FEnvironmentType := enDevelopment;
  FEnvironmentName := Environment;

  for t := Low(TEnvironmentType) to High(TEnvironmentType) do
    if SameText(EnvironmentNames[t], Environment) then
      FEnvironmentType := t;
end;

function TEnvironment.EnvironmentName: string;
begin
  Result := FEnvironmentName;
end;

function TEnvironment.EnvironmentType: TEnvironmentType;
begin
  Result := FEnvironmentType;
end;

function TEnvironment.GetSecret(Secret: string): string;
begin
  Result := FVault.GetSecret(Secret);
end;

end.
