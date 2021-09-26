unit Fido.Db.TransactionHandler.Base.Test;

interface
uses
  DUnitX.TestFramework,

  Fido.Db.TransactionHandler.Test;

type

  [TestFixture]
  TBaseTransactionHandlerTests = class(TObject)
  public
    [Test]
    procedure SingleTransactionCommitsProperly;
    [Test]
    procedure SingleTransactionRollbacksProperly;
    [Test]
    procedure NestedTransactionRollbackedReturnsFalseWhenRollBacks;
    [Test]
    procedure NestedTransactionRollbackedReturnsFalseWhenCommits;
    [Test]
    procedure NestedTransactionRollbackedReturnsTrueWhenNestedTransactionRollBacks;
    [Test]
    procedure NestedTransactionRollbackedReturnsFalseWhenNestedTransactionRollBacksAndResetNestedTransactionRollbackedStatusIsCalled;
    [Test]
    procedure NestedTransactionRollbackedReturnsFalseWhenNestedTransactionCommits;
    [Test]
    procedure NestedTransactionRollbackedReturnsFalseWhenNestedCommitsAndMainRollBacks;
    [Test]
    procedure NestedTransactionRollbackedReturnsFalseWhenAllCommit;
  end;

implementation

{ TBaseTransactionHandlerTests }

procedure TBaseTransactionHandlerTests.SingleTransactionCommitsProperly;
var
  Sut: TTestTransactionHandler;
  TransactionID: Integer;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      Sut := TTestTransactionHandler.Create;
      try
        TransactionId := Sut.Start;
        Sut.Commit(TransactionId);
      finally
        Sut.Free;
      end;
    end);
end;

procedure TBaseTransactionHandlerTests.SingleTransactionRollbacksProperly;
var
  Sut: TTestTransactionHandler;
  TransactionID: Integer;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      Sut := TTestTransactionHandler.Create;
      try

        TransactionId := Sut.Start;
        Sut.Rollback(TransactionId);
      finally
        Sut.Free;
      end;
    end);
end;

procedure TBaseTransactionHandlerTests.NestedTransactionRollbackedReturnsFalseWhenCommits;
var
  Sut: TTestTransactionHandler;
  TransactionID1: Integer;
begin
  Sut := TTestTransactionHandler.Create;
  try
    TransactionID1 := Sut.Start;
    Sut.Commit(TransactionID1);
    Assert.IsFalse(Sut.NestedTransactionRollbacked);
  finally
    Sut.Free;
  end;
end;

procedure TBaseTransactionHandlerTests.NestedTransactionRollbackedReturnsFalseWhenNestedTransactionRollBacksAndResetNestedTransactionRollbackedStatusIsCalled;
var
  Sut: TTestTransactionHandler;
  TransactionID1: Integer;
  TransactionID2: Integer;
begin
  Sut := TTestTransactionHandler.Create;
  try
    TransactionID1 := Sut.Start;
    TransactionID2 := Sut.Start;
    Sut.Rollback(TransactionID2);
    Sut.ResetNestedTransactionRollbackedStatus;
    Sut.Rollback(TransactionID1);
    Assert.IsFalse(Sut.NestedTransactionRollbacked);
  finally
    Sut.Free;
  end;
end;

procedure TBaseTransactionHandlerTests.NestedTransactionRollbackedReturnsFalseWhenRollBacks;
var
  Sut: TTestTransactionHandler;
  TransactionID1: Integer;
begin
  Sut := TTestTransactionHandler.Create;
  try
    TransactionID1 := Sut.Start;
    Sut.Rollback(TransactionID1);
    Assert.IsFalse(Sut.NestedTransactionRollbacked);
  finally
    Sut.Free;
  end;
end;

procedure TBaseTransactionHandlerTests.NestedTransactionRollbackedReturnsFalseWhenAllCommit;
var
  Sut: TTestTransactionHandler;
  TransactionID1: Integer;
  TransactionID2: Integer;
begin
  Sut := TTestTransactionHandler.Create;
  try
    TransactionID1 := Sut.Start;
    TransactionID2 := Sut.Start;
    Sut.Commit(TransactionID2);
    Sut.Commit(TransactionID1);
    Assert.IsFalse(Sut.NestedTransactionRollbacked);
  finally
    Sut.Free;
  end;
end;

procedure TBaseTransactionHandlerTests.NestedTransactionRollbackedReturnsFalseWhenNestedCommitsAndMainRollBacks;
var
  Sut: TTestTransactionHandler;
  TransactionID1: Integer;
  TransactionID2: Integer;
begin
  Sut := TTestTransactionHandler.Create;
  try
    TransactionID1 := Sut.Start;
    TransactionID2 := Sut.Start;
    Sut.Commit(TransactionID2);
    Sut.Rollback(TransactionID1);
    Assert.IsFalse(Sut.NestedTransactionRollbacked);
  finally
    Sut.Free;
  end;
end;

procedure TBaseTransactionHandlerTests.NestedTransactionRollbackedReturnsFalseWhenNestedTransactionCommits;
var
  Sut: TTestTransactionHandler;
  TransactionID2: Integer;
begin
  Sut := TTestTransactionHandler.Create;
  try
    TransactionID2 := Sut.Start;
    Sut.Commit(TransactionID2);
    Assert.IsFalse(Sut.NestedTransactionRollbacked);
  finally
    Sut.Free;
  end;
end;

procedure TBaseTransactionHandlerTests.NestedTransactionRollbackedReturnsTrueWhenNestedTransactionRollBacks;
var
  Sut: TTestTransactionHandler;
  TransactionID1: Integer;
  TransactionID2: Integer;
begin
  Sut := TTestTransactionHandler.Create;
  try
    TransactionID1 := Sut.Start;
    TransactionID2 := Sut.Start;
    Sut.Rollback(TransactionID2);
    Assert.IsTrue(Sut.NestedTransactionRollbacked);
    Sut.Rollback(TransactionID1);
  finally
    Sut.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TBaseTransactionHandlerTests);
end.
