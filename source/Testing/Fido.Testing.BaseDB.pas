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

unit Fido.Testing.BaseDB;

interface

uses
  Spring,
  Spring.Collections,

  Fido.Resource.StreamReader,
  Fido.Resource.StreamReader.Intf,
  Fido.Resource.StringReader,
  Fido.Resource.StringReader.Intf,
  Fido.Mappers,
  Fido.VirtualStatement.Intf,
  Fido.Db.Transaction,
  Fido.Db.Transaction.Intf,
  Fido.Db.Transaction.Handler.Intf;

type
  // As this test has a dependency on the database it is not supposed to be used in unit testing, but it has been
  // developed to facilitate the creation of integration tests that use the DUnitx functionalities in order to do
  // what Fitnesse/DBFit does but with the queries/commands we save as resources in our projects.
  //
  // Main advantage: Changes on the queries/commands stored as resources can be monitored and tested against errors.
  // This test ensures that at the end all of the changes are rollbacked.
  TBaseDBTest = class
  private
    FStringResReader: IStringResourceReader;
    FTransaction: ITransaction;
  protected
    function GetRecordCount(const Enumerator: IEnumerator): Integer;
    function ExtractData<TStatement: IInterface; TDto: class, constructor>(const Enumerator: IEnumerator): IReadOnlyList<TDto>;
    function GetStringResReader: IStringResourceReader;
    function GetResourceText(const ResourceName: string): string;
  public
    constructor Create(const TransactionHandler: ITransactionHandler; const StringResourceReader: IStringResourceReader);
    destructor Destroy; override;
  end;

implementation

{ TBaseDBTest }

function TBaseDBTest.GetRecordCount(const Enumerator: IEnumerator): Integer;
begin
  Result := 0;
  while Enumerator.MoveNext do
    Inc(Result);
end;

function TBaseDBTest.GetResourceText(const ResourceName: string): string;
begin
  Result := FStringResReader.GetStringResource(ResourceName);
end;

function TBaseDBTest.GetStringResReader: IStringResourceReader;
begin
  Result := FStringResReader;
end;

function TBaseDBTest.ExtractData<TStatement, TDto>(const Enumerator: IEnumerator): IReadOnlyList<TDto>;
var
  List: IList<TDto>;
  Item: TDto;
  Statement: TStatement;
begin
  List := TCollections.CreateList<TDto>;
  while Enumerator.MoveNext do
  begin
    Item := TDto.Create;
    Statement := Enumerator.Current.AsType<TStatement>;
    Mappers.Map<TStatement, TDto>(Statement, Item);
    List.Add(Item);
  end;
  Result := List.AsReadOnlyList;
end;

constructor TBaseDBTest.Create(
  const TransactionHandler: ITransactionHandler;
  const StringResourceReader: IStringResourceReader);
begin
  Guard.CheckNotNull(TransactionHandler, 'TransactionHandler');
  Guard.CheckNotNull(StringResourceReader, 'StringResourceReader');
  FStringResReader := StringResourceReader;
  FTransaction := TTransaction.Create(TransactionHandler);
end;

destructor TBaseDBTest.Destroy;
begin
  if Assigned(FTransaction) then
    FTransaction.Rollback;
  inherited;
end;

end.
