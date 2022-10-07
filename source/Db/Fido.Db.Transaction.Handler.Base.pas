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

unit Fido.Db.Transaction.Handler.Base;

interface

uses
  System.SyncObjs,

  Fido.Exceptions,
  Fido.Db.Transaction.Intf,
  Fido.Db.Transaction.Handler.Intf;

type
  TBaseTransactionHandler = class (TInterfacedObject, ITransactionHandler)
  protected
    procedure TestNesting(const Commiting: boolean; const ID: integer);

    procedure RaiseError(const Msg: string; const Args: array of const);
    procedure DoStart; virtual;
    procedure DoCommit; virtual;
    procedure DoRollback; virtual;
    procedure DoResetNestedTransactionRollbackedStatus; virtual;
    function DoGetNestedTransactionRollbacked: Boolean; virtual;
    function DoGetNestingLevel: Integer; virtual;
  public
    destructor Destroy; override;

    function Start: integer;
    procedure Commit(const ID: Integer);
    procedure Rollback(const ID: Integer);
    procedure ResetNestedTransactionRollbackedStatus;
    function NestedTransactionRollbacked: Boolean;
    function NestingLevel: Integer;
  end;

implementation

{ TBaseTransactionHandler }

procedure TBaseTransactionHandler.Commit(const ID: Integer);
begin
  TestNesting(true, ID);

  DoCommit;
end;

destructor TBaseTransactionHandler.Destroy;
begin
// this should never happen, but if the Transaction Handler goes out of scope with a transaction still open then we force the rollback
  while (DoGetNestingLevel > 0) do
    DoRollback;
  inherited;
end;

procedure TBaseTransactionHandler.DoCommit;
begin
  ; // do nothing in Null implementation; override in actual ones
end;

function TBaseTransactionHandler.DoGetNestingLevel: Integer;
begin
  Result := 0; // return 0 in Null implementation; override in actual ones
end;

procedure TBaseTransactionHandler.DoResetNestedTransactionRollbackedStatus;
begin
  ; // do nothing in Null implementation; override in actual ones
end;

procedure TBaseTransactionHandler.DoRollback;
begin
  ; // do nothing in Null implementation; override in actual ones
end;

procedure TBaseTransactionHandler.DoStart;
begin
  ; // do nothing in Null implementation; override in actual ones
end;

function TBaseTransactionHandler.NestedTransactionRollbacked: Boolean;
begin
  Result := DoGetNestedTransactionRollbacked;
end;

function TBaseTransactionHandler.NestingLevel: Integer;
begin
  Result := DoGetNestingLevel;
end;

function TBaseTransactionHandler.DoGetNestedTransactionRollbacked: Boolean;
begin
  Result := False; // return False in Null implementation; override in actual ones
end;

procedure TBaseTransactionHandler.RaiseError(
  const Msg: string;
  const Args: array of const);
begin
  // log?
  raise EFidoTransactionError.CreateFmt(Msg, Args);
end;

procedure TBaseTransactionHandler.ResetNestedTransactionRollbackedStatus;
begin
  DoResetNestedTransactionRollbackedStatus;
end;

procedure TBaseTransactionHandler.Rollback(const ID: Integer);
begin
  TestNesting(false, ID);
  DoRollback;
end;

function TBaseTransactionHandler.Start: integer;
begin
  DoStart;
  Result := DoGetNestingLevel;
  // Log?
end;

procedure TBaseTransactionHandler.TestNesting(
  const Commiting: boolean;
  const ID: integer);
const
  ActionText: array[boolean] of string = ('Rolling back', 'Committing');
begin
  Assert(DoGetNestingLevel >= 0);

  if DoGetNestingLevel = 0 then
    RaiseError('%s while not in transaction mode (no call expected, got %d',
      [ActionText[Commiting], ID])
  else
  // Closing transactions in the wrong order.
  if DoGetNestingLevel > ID then
    RaiseError('%s transaction while nested transaction is still open. Expected %d, got %d',
      [ActionText[Commiting], DoGetNestingLevel, ID])
  // Closing transactions that are not opened.
  else if DoGetNestingLevel < ID then
    RaiseError('%s transaction that was not opened or is already closed. Expected %d, got %d',
      [ActionText[Commiting], DoGetNestingLevel, ID]);
end;

end.
