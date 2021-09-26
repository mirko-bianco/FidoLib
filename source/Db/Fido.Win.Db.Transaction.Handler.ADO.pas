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

unit Fido.Win.Db.Transaction.Handler.ADO;

interface

uses
  Spring,

  Fido.Win.Db.Connections.Ado,
  Fido.Db.Transaction.Handler.Intf,
  Fido.Db.Transaction.Handler.Base;

type
  TADOTransactionHandler = class (TBaseTransactionHandler, ITransactionHandler)
  private
    FConnections: TAdoConnections;
  protected
    procedure DoStart; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
    procedure DoResetNestedTransactionRollbackedStatus; override;
    function DoGetNestedTransactionRollbacked: Boolean; override;
    function DoGetNestingLevel: Integer; override;
  public
    constructor Create(const Connections: TAdoConnections);
  end;

implementation

{ TADOTransactionHandler }

constructor TADOTransactionHandler.Create(const Connections: TAdoConnections);
begin
  Guard.CheckNotNull(Connections, 'Connections');
  FConnections := Connections;
end;

procedure TADOTransactionHandler.DoCommit;
begin
  inherited;
  FConnections.GetCurrent.CommitTrans;
end;

procedure TADOTransactionHandler.DoResetNestedTransactionRollbackedStatus;
begin
  inherited;
  FConnections.GetCurrent.ResetNestedTransactionRollbackedStatus;
end;

procedure TADOTransactionHandler.DoRollback;
begin
  inherited;
  FConnections.GetCurrent.RollbackTrans;
end;

procedure TADOTransactionHandler.DoStart;
begin
  inherited;
  FConnections.GetCurrent.BeginTrans;
end;

function TADOTransactionHandler.DoGetNestedTransactionRollbacked: Boolean;
begin
  inherited;
  Result := FConnections.GetCurrent.NestedTransactionRollbacked;
end;

function TADOTransactionHandler.DoGetNestingLevel: Integer;
begin
  inherited;
  Result := FConnections.GetCurrent.NestingLevel;
end;

end.
