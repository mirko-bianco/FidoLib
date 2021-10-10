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

unit Fido.Db.Transactional;

interface

uses
  System.SysUtils,

  Spring,

  Fido.Db.Transactional.Intf,
  Fido.Db.Transaction.Intf,
  Fido.Db.Transaction.Handler.Intf;

type
  TTransactional = class(TInterfacedObject, ITransactional)
  private
    FHandler: ITransactionHandler;
    FTransactionFactory: TFunc<ITransactionHandler, ITransaction>;
  public
    constructor Create(const Handler: ITransactionHandler; const TransactionFactory: TFunc<ITransactionHandler, ITransaction>);

    function StartTransaction: ITransaction;
    procedure ResetNestedTransactionRollbackedStatus;
    function NestedTransactionRollbacked: Boolean;
    function NestingLevel: Integer;
  end;

implementation

{ TTransactional }

constructor TTransactional.Create(const Handler: ITransactionHandler; const TransactionFactory: TFunc<ITransactionHandler, ITransaction>);
begin
  Guard.CheckNotNull(Handler, 'Handler');
  Guard.CheckTrue(Assigned(TransactionFactory), 'TransactionFactory not assigned');
  FHandler := Handler;
  FTransactionFactory := TransactionFactory;
end;

function TTransactional.StartTransaction: ITransaction;
begin
  Result := FTransactionFactory(FHandler);
end;

function TTransactional.NestedTransactionRollbacked: Boolean;
begin
  Result := FHandler.NestedTransactionRollbacked;
end;

function TTransactional.NestingLevel: Integer;
begin
  Result := FHandler.NestingLevel;
end;

procedure TTransactional.ResetNestedTransactionRollbackedStatus;
begin
  FHandler.ResetNestedTransactionRollbackedStatus;
end;

end.
