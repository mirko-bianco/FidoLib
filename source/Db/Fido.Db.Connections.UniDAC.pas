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

unit Fido.Db.Connections.UniDAC;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Uni,

  Spring,
  Spring.Collections,

  Fido.Collections.PerXDictionary.Intf,
  Fido.Collections.UpdateablePerXDictionary,

  AccessUniProvider,
  AdvantageUniProvider,
  ASEUniProvider,
  BigCommerceUniProvider,
  BigQueryUniProvider,
  DB2UniProvider,
  DBFUniProvider,
  DynamicsCRMUniProvider,
  FreshBooksUniProvider,
  HubSpotUniProvider,
  InterBaseUniProvider,
  MagentoUniProvider,
  MailChimpUniProvider,
  MongoDBUniProvider,
  MySQLUniProvider,
  NetSuiteUniProvider,
  // NexusDBUniProvider,
  ODBCUniProvider,
  OracleUniProvider,
  PostgreSQLUniProvider,
  QuickBooksUniProvider,
  RedshiftUniProvider,
  SalesforceMCUniProvider,
  SalesforceUniProvider,
  SQLiteUniProvider,
  SQLServerUniProvider,
  SugarCRMUniProvider,
  UniProvider,
  ZohoCRMUniProvider;

type
  TUniDacParams = class
  private
    FProviderName: string;
    FServerName: string;
    FDatabase: string;
    FPort: Integer;
    FUsername: string;
    FPassword: string;
    FSpecificOptions: TStringList;
  public
    property ProviderName: string read FProviderName write FProviderName;
    property ServerName: string read FServerName write FServerName;
    property Database: string read FDatabase write FDatabase;
    property Port: Integer read FPort write FPort;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property SpecificOptions: TStringList read FSpecificOptions write FSpecificOptions;

    constructor Create;
    destructor Destroy; override;
  end;

  TUniDACConnections = class
  protected type
    TFidoUniDACConnections = TUpdateablePerXDictionary<TUniConnection, TUniDacParams>;
  protected var
    FUniDACConnections: TFidoUniDACConnections;
  public
    constructor Create(const Parameters: TUniDacParams; const PerXDictionaryFactoryFunc: TFunc < TDictionaryOwnerships, TFunc<TUniConnection>, IPerXDictionary < TUniConnection >> );
    destructor Destroy; override;

    function GetCurrent: TUniConnection;
  end;

implementation

// constructor TUniDACConnections.Create(const Parameters: TUniDacParams; const CustomThreadId: TFunc<Int64> = nil);
constructor TUniDACConnections.Create(
  const Parameters: TUniDacParams;
  const PerXDictionaryFactoryFunc: TFunc < TDictionaryOwnerships, TFunc<TUniConnection>, IPerXDictionary < TUniConnection >> );
begin
  inherited Create;
  FUniDACConnections := TUpdateablePerXDictionary<TUniConnection, TUniDacParams>.Create(
    PerXDictionaryFactoryFunc,
    [doOwnsValues],
    function: TUniConnection
    begin
      Guard.CheckFalse(FUniDACConnections.GetUpdateableValue.ProviderName.Trim.IsEmpty, 'Providername is null');
      Guard.CheckFalse(FUniDACConnections.GetUpdateableValue.ServerName.Trim.IsEmpty, 'Servername is null');
      Guard.CheckFalse(FUniDACConnections.GetUpdateableValue.Database.Trim.IsEmpty, 'Database is null');

      Result := TUniConnection.Create(nil);
      Result.ProviderName := FUniDACConnections.GetUpdateableValue.ProviderName;
      Result.Server := FUniDACConnections.GetUpdateableValue.ServerName;
      Result.Database := FUniDACConnections.GetUpdateableValue.Database;
      if FUniDACConnections.GetUpdateableValue.Port > 0 then
        Result.Port := FUniDACConnections.GetUpdateableValue.Port;
      if not FUniDACConnections.GetUpdateableValue.Username.Trim.IsEmpty then
        Result.Username := FUniDACConnections.GetUpdateableValue.Username;
      if not FUniDACConnections.GetUpdateableValue.Password.Trim.IsEmpty then
        Result.Password := FUniDACConnections.GetUpdateableValue.Password;
      Result.LoginPrompt := False;

      Result.SpecificOptions.Clear;
      for var s in FUniDACConnections.GetUpdateableValue.SpecificOptions do
      begin
        Result.SpecificOptions.Values[s.Split(['='])[0]] := s.Split(['='])[1];
      end;
    end,
    procedure(Connection: TUniConnection; Params: TUniDacParams)
    begin
      Connection.ProviderName := Params.ProviderName;
      Connection.Server := Params.ServerName;
      Connection.Database := Params.Database;
      if Params.Port > 0 then
        Connection.Port := Params.Port;
      if not Params.Username.Trim.IsEmpty then
        Connection.Username := Params.Username;
      if not Params.Password.Trim.IsEmpty then
        Connection.Password := Params.Password;

      Connection.SpecificOptions.Clear;
      for var s in FUniDACConnections.GetUpdateableValue.SpecificOptions do
      begin
        Connection.SpecificOptions.Values[s.Split(['='])[0]] := s.Split(['='])[1];
      end;
    end);
  FUniDACConnections.SetUpdateableValue(Parameters);
end;

destructor TUniDACConnections.Destroy;
begin
  FUniDACConnections.Free;
  inherited;
end;

function TUniDACConnections.GetCurrent: TUniConnection;
begin
  Result := FUniDACConnections.GetCurrent;
  if not Result.Connected then
  begin
    Result.Connect;
    {$if defined(DEBUG)}
    Result.Debug := True;
    {$ifend}
  end;
end;

{ TUniDacParams }

constructor TUniDacParams.Create;
begin
  FSpecificOptions := TStringList.Create;
end;

destructor TUniDacParams.Destroy;
begin
  FSpecificOptions.Free;
  inherited;
end;

end.
