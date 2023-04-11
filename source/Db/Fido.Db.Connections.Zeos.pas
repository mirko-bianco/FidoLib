(*
 * Copyright 2023 Mirko Bianco (email: writetomirko@gmail.com)
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

unit Fido.Db.Connections.Zeos;

interface

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.Generics.Collections,

  ZAbstractConnection,
  ZConnection,

  Spring,
  Spring.Collections,

  Fido.Collections.PerXDictionary.Intf,
  Fido.Collections.UpdateablePerXDictionary;

type
  TZeosConnections = class
  protected type
    TFidoZeosConnections = TUpdateablePerXDictionary<TZConnection, TStrings>;
  protected var
    FZeosConnections: TFidoZeosConnections;
  private
    procedure OnAfterDisconnect(Sender: TObject);
    procedure CleanParams(const Params: TStrings);
  public
    constructor Create(const Parameters: TStrings; const PerXDictionaryFactoryFunc: Func<TDictionaryOwnerships, Func<TZConnection>, IPerXDictionary<TZConnection>>);
    destructor Destroy; override;

    function GetCurrent: TZConnection;
  end;

implementation

procedure TZeosConnections.CleanParams(const Params: TStrings);
  procedure DeleteIfExists(const Name: string);
    var
    Index: Integer;
  begin
    Index := Params.IndexOfName(Name);
    if Index > -1 then
      Params.Delete(Index);
  end;
begin
  DeleteIfExists('DriverID');
  DeleteIfExists('Catalog');
  DeleteIfExists('ClientCodePage');
  DeleteIfExists('Database');
  DeleteIfExists('Server');
  DeleteIfExists('LibraryLocation');
  DeleteIfExists('Password');
  DeleteIfExists('Port');
  DeleteIfExists('User_Name');
end;

constructor TZeosConnections.Create(
  const Parameters: TStrings;
  const PerXDictionaryFactoryFunc: Func<TDictionaryOwnerships, Func<TZConnection>, IPerXDictionary<TZConnection>>);
var
  Value: string;
begin
  inherited Create;
  FZeosConnections := TUpdateablePerXDictionary<TZConnection, TStrings>.Create(
    PerXDictionaryFactoryFunc,
    [doOwnsValues],
    function: TZConnection
    begin
      Result := TZConnection.Create(nil);
      var Params: Shared<TStringList> := TStringList.Create;
      Params.Value.AddStrings(FZeosConnections.GetUpdateableValue);

      Result.Protocol := Params.Value.Values['DriverID'];
      Result.Catalog := Params.Value.Values['Catalog'];
      Result.ClientCodePage := Params.Value.Values['ClientCodePage'];
      Value := Params.Value.Values['Database'];
      if not Value.IsEmpty then
        Result.Database := Value;
      Result.HostName := Params.Value.Values['Server'];
      Value := Params.Value.Values['LibraryLocation'];
      if not Value.IsEmpty then
        Result.LibraryLocation := Value;
      Result.Password := Params.Value.Values['Password'];
      Result.Port := StrToIntDef(Params.Value.Values['Port'], 0);
      Result.User := Params.Value.Values['User_Name'];

      CleanParams(Params);

      Result.Properties.Clear;

      Result.Properties.AddStrings(Params);

      Result.LoginPrompt := False;

      Result.AfterDisconnect := OnAfterDisconnect;
    end,
    procedure(const Connection: TZConnection; const Params: TStrings)
    begin
      var LParams: Shared<TStringList> := TStringList.Create;
      LParams.Value.AddStrings(Params);

      CleanParams(LParams);

      Connection.Properties.Clear;
      Connection.Properties.AddStrings(LParams);
    end);
  FZeosConnections.SetUpdateableValue(Parameters);
end;

destructor TZeosConnections.Destroy;
begin
  FZeosConnections.Free;
  inherited;
end;

function TZeosConnections.GetCurrent: TZConnection;
begin
  Result := FZeosConnections.GetCurrent;
  if not Result.Connected then
    Result.Connect;
end;

procedure TZeosConnections.OnAfterDisconnect(Sender: TObject);
begin
  FZeosConnections.ReleaseCurrent;
end;

end.
