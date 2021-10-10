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

unit Fido.Db.Transaction;

interface

uses
  System.SysUtils,

  Fido.Db.Transaction.Intf,
  Fido.Db.Transaction.Handler.Intf;

type
  TTransaction = class(TInterfacedObject, ITransaction)
  private const
    CLOSEID = -1;
  private
    FId: integer;
    FHandler: ITransactionHandler;

    procedure Close;
    function GetIsClosed: boolean;
    procedure AssertClosedState(const ExpectedState: boolean);
  public
    constructor Create(const Handler: ITransactionHandler);
    destructor Destroy; override;
    // ITransaction
    procedure Commit;
    procedure Rollback;
    procedure RunAndCommit(const Proc: TProc);
  end;

implementation

{ TTransaction }

procedure TTransaction.AssertClosedState(const ExpectedState: boolean);
const
  TransactionStateDesc: array[boolean] of string = ('closed', 'active');
begin
  if GetIsClosed <> ExpectedState then
    raise EFidoTransactionError.CreateFmt(
      'Invalid transaction state: expected "%s", actual "%s"',
      [TransactionStateDesc[ExpectedState], TransactionStateDesc[GetIsClosed]]);
end;

procedure TTransaction.Close;
begin
  FId := CLOSEID;
end;

procedure TTransaction.Commit;
begin
  AssertClosedState(false);
  FHandler.Commit(FId);
  Close;
end;

constructor TTransaction.Create(const Handler: ITransactionHandler);
begin
  inherited Create;
  FHandler := Handler;
  FId := Handler.Start;

  // make sure the handler returns 0+
  AssertClosedState(false);
end;

destructor TTransaction.Destroy;
var
  NestingLevel: Integer;
begin
  // make sure it was closed explicitly
  try
    if not GetIsClosed then
    begin
      // The transaction is misused. The transaction handler will perform a rollback on destroy
      Close;
      NestingLevel := FHandler.NestingLevel;
      // TTransaction will leak, at least let's free the handler
      FHandler := nil;
      raise EFidoTransactionError.CreateFmt(
        'Transaction went out of scope without closing. Current level %d, transaction level %d',
        [NestingLevel, FId]);
    end;
  finally
    inherited;
  end;
end;

function TTransaction.GetIsClosed: boolean;
begin
  Result := FId = CLOSEID;
end;

procedure TTransaction.Rollback;
begin
  AssertClosedState(false);
  FHandler.Rollback(FId);
  Close;
end;

procedure TTransaction.RunAndCommit(const Proc: TProc);
begin
  try
    Proc;
    Commit;
  except
    Rollback;
    raise;
  end;
end;

end.
