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

 unit Fido.Db.ScriptRunner.FireDac;

interface

uses
  System.Classes,

  FireDAC.UI.Intf,
  FireDAC.Stan.Async,
  FireDAC.Comp.ScriptCommands,
  FireDAC.Stan.Util,
  FireDAC.Stan.Intf,
  FireDAC.Comp.Script,

  Spring,

  Fido.Db.Connections.FireDac,
  Fido.Db.ScriptRunner.Intf;

type
  TFireDacDatabaseScriptRunner = class(TInterfacedObject, IDatabaseScriptRunner)
  private
    FScriptExecutor: TFDScript;
  public
    constructor Create(const Connections: TFireDacConnections);
    destructor Destroy; override;

    procedure Execute(const Script: TStrings);
  end;

implementation

{ TFireDacDatabaseScriptRunner }

constructor TFireDacDatabaseScriptRunner.Create(const Connections: TFireDacConnections);
begin
  inherited Create;
  Guard.CheckNotNull(Connections, 'Connections');
  FScriptExecutor := TFDScript.Create(nil);
  FScriptExecutor.Connection := Connections.GetCurrent;
  FScriptExecutor.ScriptOptions.BreakOnError := True;
  FScriptExecutor.ScriptOptions.IgnoreError := False;
end;

destructor TFireDacDatabaseScriptRunner.Destroy;
begin
  FScriptExecutor.Free;
  inherited;
end;

procedure TFireDacDatabaseScriptRunner.Execute(const Script: TStrings);
begin
  FScriptExecutor.ExecuteScript(Script);
end;

end.
