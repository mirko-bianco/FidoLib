unit Fido.DesignPatterns.Retries.Test;

interface

uses
  DUnitX.TestFramework,
  SysUtils,

  Spring,

  Fido.Exceptions,
  Fido.DesignPatterns.Retries;

type
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

        end);
    end,
    EFidoTestException);
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
        end);
    end,
    EFidoTestException);
end;

initialization
  TDUnitX.RegisterTestFixture(TRetriesTests);
end.

