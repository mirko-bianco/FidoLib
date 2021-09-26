unit Fido.Db.Transaction.Test;

interface
uses
  DUnitX.TestFramework,

  Fido.Exceptions,
  Fido.Db.Transaction.Intf,
  Fido.Db.Transaction,

  Fido.Db.TransactionHandler.Test;

type

  [TestFixture]
  TTransactionTests = class(TObject)
  public
    [Test]
    procedure CommitWorksWhenCalledOnce;
    [Test]
    procedure RollBackWorksWhenCalledOnce;
    [Test]
    procedure RaisesAnExceptionWhenDestroyedAndNotClosed;
    [Test]
    procedure RaisesAnExceptionWhenCommitAfterIsClosed;
    [Test]
    procedure RaisesAnExceptionWhenRollBackAfterIsClosed;
  end;

implementation

{ TTransactionTests }

procedure TTransactionTests.CommitWorksWhenCalledOnce;
var
  Sut: TTransaction;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      Sut := TTransaction.Create(TTestTransactionHandler.Create);
      try
        Sut.Commit;
      finally
        Sut.Free;
      end;
    end);
end;

procedure TTransactionTests.RaisesAnExceptionWhenCommitAfterIsClosed;
var
  Sut: TTransaction;
begin
  Assert.WillRaise(
    procedure
    begin
      Sut := TTransaction.Create(TTestTransactionHandler.Create);
      try
        Sut.Commit;
        Sut.Commit;
      finally
        Sut.Free;
      end;
    end,
    EFidoTransactionError);
end;

procedure TTransactionTests.RaisesAnExceptionWhenDestroyedAndNotClosed;
var
  Sut: TTransaction;
begin
  Assert.WillRaise(
    procedure
    begin
      Sut := TTransaction.Create(TTestTransactionHandler.Create);
      Sut.Free;
    end,
    EFidoTransactionError);
end;

procedure TTransactionTests.RaisesAnExceptionWhenRollBackAfterIsClosed;
var
  Sut: TTransaction;
begin
  Assert.WillRaise(
    procedure
    begin
      Sut := TTransaction.Create(TTestTransactionHandler.Create);
      try
        Sut.Commit;
        Sut.RollBack;
      finally
        Sut.Free;
      end;
    end,
    EFidoTransactionError);
end;

procedure TTransactionTests.RollBackWorksWhenCalledOnce;
var
  Sut: TTransaction;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      Sut := TTransaction.Create(TTestTransactionHandler.Create);
      try
        Sut.RollBack;
      finally
        Sut.Free;
      end;
    end);
end;

initialization
  TDUnitX.RegisterTestFixture(TTransactionTests);
end.
