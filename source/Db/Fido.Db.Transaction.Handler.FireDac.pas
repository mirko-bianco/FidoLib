(*
 * Copyright 2022 Mirko Bianco (email: writetomirko@gmail.com)
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

unit Fido.Db.Transaction.Handler.FireDac;

interface

uses
  System.SyncObjs,

  Fido.Utilities,
  Fido.Exceptions,
  Fido.Db.Connections.FireDac,
  Fido.Db.Transaction.Intf,
  Fido.Db.Transaction.Handler.Base,
  Fido.Db.Transaction.Handler.Intf;

type
  TFireDacTransactionHandler = class(TBaseTransactionHandler, ITransactionHandler)
  private
    FFireDacConnections: TFireDacConnections;
  protected
    procedure DoStart; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
    procedure DoResetNestedTransactionRollbackedStatus; override;
    function DoGetNestedTransactionRollbacked: Boolean; override;
    function DoGetNestingLevel: Integer; override;
  public
    constructor Create(FireDacConnections: TFireDacConnections);
  end;

implementation

{ TFireDacTransactionHandler }

constructor TFireDacTransactionHandler.Create(FireDacConnections: TFireDacConnections);
begin
  inherited Create;
  FFireDacConnections := Utilities.CheckNotNullAndSet(FireDacConnections, 'FireDacConnections');
end;

procedure TFireDacTransactionHandler.DoCommit;
begin
  inherited;

  FFireDacConnections.GetCurrent.Commit;
end;

function TFireDacTransactionHandler.DoGetNestedTransactionRollbacked: Boolean;
begin
  Result := False;
end;

function TFireDacTransactionHandler.DoGetNestingLevel: Integer;
begin
  Result := 0;
end;

procedure TFireDacTransactionHandler.DoResetNestedTransactionRollbackedStatus;
begin
  ;
end;

procedure TFireDacTransactionHandler.DoRollback;
begin
  FFireDacConnections.GetCurrent.Rollback;
end;

procedure TFireDacTransactionHandler.DoStart;
begin
  FFireDacConnections.GetCurrent.StartTransaction;
end;

end.
