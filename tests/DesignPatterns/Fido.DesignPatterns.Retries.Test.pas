unit Fido.DesignPatterns.Retries.Test;

interface

uses
  DUnitX.TestFramework,
  SysUtils,

  Spring,

  Fido.Exceptions,
  Fido.DesignPatterns.Retries;

type
  ERetriesTestsSupported = class(EFidoException);
  ERetriesTestsNotSupported = class(EFidoException);

  [TestFixture]
  TRetriesTests = class
  public
    [Test]
    procedure RunFuncCallsFuncOnceWhenThereIsNoFailure;

    [Test]
    procedure RunFuncDoesNotRaiseEFidoTestExceptionWhenThereAreLessFailuresThanMax;

    [Test]
    procedure RunFuncRaisesEFidoTestExceptionWhenThereAreMoreOrEqualFailuresThanMax;

    [Test]
    procedure RunProcCallsProcOnceWhenThereIsNoFailure;

    [Test]
    procedure RunProcDoesNotRaiseEFidoTestExceptionWhenThereAreLessFailuresThanMax;

    [Test]
    procedure RunProcRaisesEFidoTestExceptionWhenThereAreMoreOrEqualFailuresThanMax;

    [Test]
    procedure RunFuncRaisesEFidoTestExceptionWhenItIsNotPartOfTheManagedExceptions;

    [Test]
    procedure RunProcRaisesEFidoTestExceptionWhenItIsNotPartOfTheManagedExceptions;

    [Test]
    procedure RunProcRaisesEFidoTestExceptionWhenItIsNotPartOfTheManagedExceptionsAndItIsRaisedOnSubsequentAttempt;

    [Test]
    procedure RunFuncRaisesEFidoTestExceptionWhenItIsNotPartOfTheManagedExceptionsAndItIsRaisedOnSubsequentAttempt;
  end;

implementation

{ TRetriesTests }

procedure TRetriesTests.RunFuncDoesNotRaiseEFidoTestExceptionWhenThereAreLessFailuresThanMax;
var
  Result: Boolean;
  Count: Integer;
begin
  Count := 0;

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := Retries.Run<Boolean>(
        function: Boolean
        begin
          Inc(Count);
          if Count < 3 then
            raise EFidoTestException.Create('Error Message');
          Result := True;
        end,
        function(const Exc: Exception): Boolean
        begin
          Result := Exc.InheritsFrom(EFidoTestException);
        end);
    end);

  Assert.AreEqual(True, Result);
end;

procedure TRetriesTests.RunFuncRaisesEFidoTestExceptionWhenThereAreMoreOrEqualFailuresThanMax;
var
  Result: Boolean;
  Count: Integer;
begin
  Count := 0;

  Assert.WillRaise(
    procedure
    begin
      Result := Retries.Run<Boolean>(
        function: Boolean
        begin
          Inc(Count);
          if Count <= 3 then
            raise EFidoTestException.Create('Error Message');
          Result := True;
        end,
        function(const Exc: Exception): Boolean
        begin
          Result := Exc.InheritsFrom(EFidoTestException);
        end);
    end,
    EFidoTestException);
end;

procedure TRetriesTests.RunFuncRaisesEFidoTestExceptionWhenItIsNotPartOfTheManagedExceptions;
var
  Result: Boolean;
begin
  Assert.WillRaise(
    procedure
    begin
      Result := Retries.Run<Boolean>(
        function: Boolean
        begin
          raise EFidoTestException.Create('Error Message');
        end);
    end,
    EFidoTestException);
end;

procedure TRetriesTests.RunProcRaisesEFidoTestExceptionWhenItIsNotPartOfTheManagedExceptions;
begin
  Assert.WillRaise(
    procedure
    begin
      Retries.Run(
        procedure
        begin
          raise EFidoTestException.Create('Error Message');
        end);
    end,
    EFidoTestException);
end;

procedure TRetriesTests.RunProcRaisesEFidoTestExceptionWhenItIsNotPartOfTheManagedExceptionsAndItIsRaisedOnSubsequentAttempt;
var
  Count: Integer;
begin
  Assert.WillRaise(
    procedure
    begin
      Count := 0;
      Retries.Run(
        procedure
        begin
          Inc(Count);
          if Count = 1 then
            raise ERetriesTestsSupported.Create('Error Message')
          else
            raise ERetriesTestsNotSupported.Create('Error Message');
        end,
        function(const Exc: Exception): Boolean
        begin
          Result := Exc.InheritsFrom(ERetriesTestsSupported);
        end);
    end,
    ERetriesTestsNotSupported);
end;

procedure TRetriesTests.RunFuncRaisesEFidoTestExceptionWhenItIsNotPartOfTheManagedExceptionsAndItIsRaisedOnSubsequentAttempt;
var
  Count: Integer;
begin
  Assert.WillRaise(
    procedure
    begin
      Count := 0;
      Retries.Run<Boolean>(
        function: Boolean
        begin
          Inc(Count);
          if Count = 1 then
            raise ERetriesTestsSupported.Create('Error Message')
          else
            raise ERetriesTestsNotSupported.Create('Error Message');
        end,
        function(const Exc: Exception): Boolean
        begin
          Result := Exc.InheritsFrom(ERetriesTestsSupported);
        end);
    end,
    ERetriesTestsNotSupported);
end;

procedure TRetriesTests.RunFuncCallsFuncOnceWhenThereIsNoFailure;
var
  Result: Boolean;
  Count: Integer;
begin
  Count := 0;

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := Retries.Run<Boolean>(
        function: Boolean
        begin
          Result := True;
          Inc(Count);
        end);
    end);

  Assert.AreEqual(1, Count);
  Assert.AreEqual(True, Result);
end;

procedure TRetriesTests.RunProcCallsProcOnceWhenThereIsNoFailure;
var
  Count: Integer;
begin
  Count := 0;

  Assert.WillNotRaiseAny(
    procedure
    begin
      Retries.Run(
        Procedure
        begin
          Inc(Count);
        end);
    end);

  Assert.AreEqual(1, Count);
end;

procedure TRetriesTests.RunProcDoesNotRaiseEFidoTestExceptionWhenThereAreLessFailuresThanMax;
var
  Count: Integer;
begin
  Count := 0;

  Assert.WillNotRaiseAny(
    procedure
    begin
      Retries.Run(
        Procedure
        begin
          Inc(Count);
          if Count < 3 then
            raise EFidoTestException.Create('Error Message');
        end,
        function(const Exc: Exception): Boolean
        begin
          Result := Exc.InheritsFrom(EFidoTestException);
        end);
    end);
end;

procedure TRetriesTests.RunProcRaisesEFidoTestExceptionWhenThereAreMoreOrEqualFailuresThanMax;
var
  Count: Integer;
begin
  Count := 0;

  Assert.WillRaise(
    procedure
    begin
      Retries.Run(
        procedure
        begin
          Inc(Count);
          if Count <= 3 then
            raise EFidoTestException.Create('Error Message');
        end,
        function(const Exc: Exception): Boolean
        begin
          Result := Exc.InheritsFrom(EFidoTestException);
        end);
    end,
    EFidoTestException);
end;

initialization
  TDUnitX.RegisterTestFixture(TRetriesTests);
end.

