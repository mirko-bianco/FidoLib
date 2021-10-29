unit Fido.DesignPatterns.Retries.Test;

interface

uses
  DUnitX.TestFramework,
  SysUtils,

  Spring,

  Fido.DesignPatterns.Retries;

type
  [TestFixture]
  TRetriesTests = class
  public
    [Test]
    procedure RunFuncCallsFuncOnceWhenThereIsNoFailure;

    [Test]
    procedure RunFuncDoesNotRaiseExceptionWhenThereAreLessFailuresThanMax;

    [Test]
    procedure RunFuncRaisesExceptionWhenThereAreMoreOrEqualFailuresThanMax;

    [Test]
    procedure RunProcCallsProcOnceWhenThereIsNoFailure;

    [Test]
    procedure RunProcDoesNotRaiseExceptionWhenThereAreLessFailuresThanMax;

    [Test]
    procedure RunProcRaisesExceptionWhenThereAreMoreOrEqualFailuresThanMax;
  end;

implementation

{ TRetriesTests }

procedure TRetriesTests.RunFuncDoesNotRaiseExceptionWhenThereAreLessFailuresThanMax;
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
            raise Exception.Create('Error Message');
          Result := True;
        end);
    end);

  Assert.AreEqual(True, Result);
end;

procedure TRetriesTests.RunFuncRaisesExceptionWhenThereAreMoreOrEqualFailuresThanMax;
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
            raise Exception.Create('Error Message');
          Result := True;

        end);
    end,
    Exception);
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

procedure TRetriesTests.RunProcDoesNotRaiseExceptionWhenThereAreLessFailuresThanMax;
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
            raise Exception.Create('Error Message');
        end);
    end);
end;

procedure TRetriesTests.RunProcRaisesExceptionWhenThereAreMoreOrEqualFailuresThanMax;
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
            raise Exception.Create('Error Message');
        end);
    end,
    Exception);
end;

initialization
  TDUnitX.RegisterTestFixture(TRetriesTests);
end.

