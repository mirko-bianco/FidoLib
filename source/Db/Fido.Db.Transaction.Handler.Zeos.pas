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

unit Fido.Db.Transaction.Handler.Zeos;

interface

uses
  System.SyncObjs,

  Fido.Utilities,
  Fido.Exceptions,
  Fido.Db.Connections.Zeos,
  Fido.Db.Transaction.Intf,
  Fido.Db.Transaction.Handler.Base,
  Fido.Db.Transaction.Handler.Intf;

type
  TZeosTransactionHandler = class(TBaseTransactionHandler, ITransactionHandler)
  private
    FZeosConnections: TZeosConnections;
  protected
    procedure DoStart; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
    procedure DoResetNestedTransactionRollbackedStatus; override;
    function DoGetNestedTransactionRollbacked: Boolean; override;
    function DoGetNestingLevel: Integer; override;
  public
    constructor Create(ZeosConnections: TZeosConnections);
  end;

implementation

{ TZeosTransactionHandler }

constructor TZeosTransactionHandler.Create(ZeosConnections: TZeosConnections);
begin
  inherited Create;
  FZeosConnections := Utilities.CheckNotNullAndSet(ZeosConnections, 'ZeosConnections');
end;

procedure TZeosTransactionHandler.DoCommit;
begin
  inherited;

  FZeosConnections.GetCurrent.Commit;
end;

function TZeosTransactionHandler.DoGetNestedTransactionRollbacked: Boolean;
begin
  Result := False;
end;

function TZeosTransactionHandler.DoGetNestingLevel: Integer;
begin
  Result := 0;
end;

procedure TZeosTransactionHandler.DoResetNestedTransactionRollbackedStatus;
begin
  ;
end;

procedure TZeosTransactionHandler.DoRollback;
begin
  FZeosConnections.GetCurrent.Rollback;
end;

procedure TZeosTransactionHandler.DoStart;
begin
  FZeosConnections.GetCurrent.StartTransaction;
end;

end.
