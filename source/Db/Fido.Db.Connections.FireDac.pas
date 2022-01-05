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

unit Fido.Db.Connections.FireDac;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  FireDAC.Comp.Client,
  FireDAC.Stan.Intf,

  Spring.Collections,

  Fido.Collections.PerXDictionary.Intf,
  Fido.Collections.UpdateablePerXDictionary;

type
  TFireDacConnections = class
  protected type
    TFidoFireDacConnections = TUpdateablePerXDictionary<TFDConnection, TStrings>;
  protected var
    FFireDacConnections: TFidoFireDacConnections;
  private
    procedure OnAfterDisconnect(Sender: TObject);
  public
    constructor Create(const Parameters: TStrings; const PerXDictionaryFactoryFunc: TFunc<TDictionaryOwnerships, TFunc<TFDConnection>, IPerXDictionary<TFDConnection>>);
    destructor Destroy; override;

    function GetCurrent: TFDConnection;
  end;

implementation

constructor TFireDacConnections.Create(
  const Parameters: TStrings;
  const PerXDictionaryFactoryFunc: TFunc<TDictionaryOwnerships, TFunc<TFDConnection>, IPerXDictionary<TFDConnection>>);
begin
  inherited Create;
  FFireDacConnections := TUpdateablePerXDictionary<TFDConnection, TStrings>.Create(
    PerXDictionaryFactoryFunc,
    [doOwnsValues],
    function: TFDConnection
    begin
      Result := TFDConnection.Create(nil);
      Result.Params.Clear;
      Result.Params.AddStrings(FFireDacConnections.GetUpdateableValue);
      Result.LoginPrompt := False;
      with Result.FormatOptions.MapRules.Add do
      begin
        SourceDataType := dtByteString;
        SizeMin := 16;
        SizeMax := 16;
        TargetDataType := dtGUID;
      end;
      Result.AfterDisconnect := OnAfterDisconnect;
    end,
    procedure(Connection: TFDConnection; Params: TStrings)
    begin
      Connection.Params.Clear;
      Connection.Params.AddStrings(Params);
    end);
  FFireDacConnections.SetUpdateableValue(Parameters);
end;

destructor TFireDacConnections.Destroy;
begin
  FFireDacConnections.Free;
  inherited;
end;

function TFireDacConnections.GetCurrent: TFDConnection;
begin
  Result := FFireDacConnections.GetCurrent;
  if not Result.Connected then
    Result.Open;
end;

procedure TFireDacConnections.OnAfterDisconnect(Sender: TObject);
begin
  FFireDacConnections.ReleaseCurrent;
end;

end.
