(*
 * Copyright 2012 Mirko Bianco (email: mirko.bianco.work@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

unit Fido.win.Db.Connection.NestedTransactions.Ado;

interface

uses
  AdoDB, AdoInt, Classes,

  Fido.Exceptions;

type
  TPSExecuteEvent = procedure(Sender: TObject; SQL: WideString) of object;

  EFidoAdoNestedTransactionException = class(EFidoException);

  TADONestedTransactionsConnection = class(TADOConnection)
  private
    FNestingLevel: Integer;
    FOnPSExecute: TPSExecuteEvent;
    FNestedTransactionRollbacked: Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    procedure StartTransaction;
    procedure RollbackTransaction;
    procedure CommitTransaction;
    procedure ResetNestedTransactionRollbackedStatus;

    property NestingLevel: Integer read FNestingLevel;
    property NestedTransactionRollbacked: Boolean read FNestedTransactionRollbacked;
  published
    property OnPSExecute: TPSExecuteEvent read FOnPSExecute write FOnPSExecute;
    property Connected stored False;
    property ConnectionString stored False;
  end;

implementation

uses
  SysUtils;

constructor TADONestedTransactionsConnection.Create(AOwner: TComponent);
begin
  inherited;

  FOnPSExecute := nil;

  // Initialize the nesting level
  FNestingLevel := 0;
  FNestedTransactionRollbacked := False;
end;

procedure TADONestedTransactionsConnection.StartTransaction;
begin
  // If no transaction has been started yet, begin one
  if FNestingLevel = 0 then
  begin

    // Start a transaction. If already in transaction, an invalid situation is
    // created so raise an exception
    if not NestingLevel = 0 then
      StartTransaction
    else
      raise EFidoAdoNestedTransactionException.Create('Connection already in transaction');

  end;

  // Increase transaction nesting level
  Inc(FNestingLevel);
end;

procedure TADONestedTransactionsConnection.CommitTransaction;
begin
  // If no transaction is currently set, raise exception
  if (FNestingLevel = 0) or (NestingLevel = 0) then
    raise EFidoAdoNestedTransactionException.Create('No current transaction');

  // First decrease nesting level
  Dec(FNestingLevel);

  // If last transaction nesting level was committed, commit all
  if FNestingLevel = 0 then
    CommitTransaction;

  if FNestingLevel = 0 then
    FNestedTransactionRollbacked := False;
end;

procedure TADONestedTransactionsConnection.ResetNestedTransactionRollbackedStatus;
begin
  FNestedTransactionRollbacked := False;
end;

procedure TADONestedTransactionsConnection.RollbackTransaction;
begin
  // If no transaction is currently set, raise exception
  if (FNestingLevel = 0) or (NestingLevel = 0) then
    raise EFidoAdoNestedTransactionException.Create('No current transaction');

  // First decrease nesting level
  Dec(FNestingLevel);

  FNestedTransactionRollbacked := FNestingLevel > 0;

  // If last transaction nesting level was committed, commit all
  if FNestingLevel = 0 then
    RollbackTransaction;
end;

end.

