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

unit Fido.Win.Db.Connections.Ado;

interface

uses
  Windows,
  System.SysUtils,
  Data.Win.ADODB,
  System.Generics.Collections,

  Spring.Collections,

  Fido.Win.Db.Connection.NestedTransactions.Ado,
  Fido.Collections.PerXDictionary.Intf,
  Fido.Collections.UpdateablePerXDictionary;

type
  TAdoConnections = class
  private type
    TFidoAdoConnections = TUpdateablePerXDictionary<TADONestedTransactionsConnection, string>;
  private var
    FAdoConnections: TFidoAdoConnections;
  public
    constructor Create(const ConnectionString: string;
      const PerXDictionaryFactoryFunc: TFunc<TDictionaryOwnerships, TFunc<TADONestedTransactionsConnection>, IPerXDictionary<TADONestedTransactionsConnection>>);
    destructor Destroy; override;

    function GetCurrent: TADONestedTransactionsConnection;
  end;

implementation

constructor TAdoConnections.Create(
  const ConnectionString: string;
  const PerXDictionaryFactoryFunc: TFunc<TDictionaryOwnerships, TFunc<TADONestedTransactionsConnection>, IPerXDictionary<TADONestedTransactionsConnection>>);
begin
  inherited Create;

  FAdoConnections := TFidoAdoConnections.Create(
    PerXDictionaryFactoryFunc,
    [doOwnsValues],
    function: TADONestedTransactionsConnection
    begin
      Result := TADONestedTransactionsConnection.Create(nil);
      Result.ConnectionString := FAdoConnections.GetUpdateableValue;
      Result.CommandTimeout := 90;
      Result.ConnectionTimeout := 30;
      Result.CursorLocation := clUseClient;
      Result.IsolationLevel := ilReadCommitted;
      Result.KeepConnection := True;
      Result.LoginPrompt := False;
      Result.Mode := cmReadWrite;
      Result.Provider := 'OraOLEDB.Oracle.1';
    end,
    procedure(Connection: TADONestedTransactionsConnection; ConnectionString: string)
    begin
      Connection.ConnectionString := ConnectionString;
    end);

    FAdoConnections.SetUpdateableValue(ConnectionString);
end;

destructor TAdoConnections.Destroy;
begin
  FAdoConnections.Free;
  inherited;
end;

function TAdoConnections.GetCurrent: TADONestedTransactionsConnection;
begin
  Result := FAdoConnections.GetCurrent;
end;

end.
