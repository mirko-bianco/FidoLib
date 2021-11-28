unit Fido.Async.Procs.Test;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Rtti,

  Fido.Exceptions,
  Fido.Boxes,
  Fido.Async.Procs;

type
  [TestFixture]
  TAsyncProcsTests = class
  public
    [Test]
    procedure AsyncProcsWithOneStepAndResolveWaitsAndFinishes;

    [Test]
    procedure AsyncProcsWithOneStepAndResolveSetsStatusToCancelledWhenItExpires;

    [Test]
    procedure AsyncProcsWithOneStepAndResolveThatRaisesAnEFidoTestExceptionSetStatusToFailed;

    [Test]
    procedure AsyncProcsWithOneStepAndManagedCatchAndResolveThatRaisesAnEFidoTestExceptionSetStatusToFinished;

    [Test]
    procedure AsyncProcsWithMultipleStepsAndResolveWaitsAndFinishes;

    [Test]
    procedure AsyncProcsWithMultipleStepsAndResolveThatRaisesAnEFidoTestExceptionSetStatusToFailed;

    [Test]
    procedure AsyncProcsWithMultipleStepsAndManagedCatchAndResolveThatRaisesAnEFidoTestExceptionSetStatusToFinished;

    [Test]
    procedure AsyncProcsWithMultipleStepsAndResolveSetsStatusToCancelledWhenItExpires;
  end;

implementation

{ TAsyncProcsTests }

procedure TAsyncProcsTests.AsyncProcsWithOneStepAndResolveWaitsAndFinishes;
var
  Result: TAsyncProcStatus;
begin
  Result := AsyncProcs.
    Queue(
      procedure
      begin
        Sleep(25);
      end).
    Run.
    Resolve;

  Assert.AreEqual(TAsyncProcstatus.Finished, Result);
end;

procedure TAsyncProcsTests.AsyncProcsWithMultipleStepsAndResolveWaitsAndFinishes;
var
  Result: TAsyncProcStatus;
begin
  Result := AsyncProcs.
    Queue(
      procedure
      begin
        Sleep(25);
      end).
    &Then(
      procedure
      begin
        Sleep(25);
      end).
    Run.
    Resolve;

  Assert.AreEqual(TAsyncProcstatus.Finished, Result);
end;

procedure TAsyncProcsTests.AsyncProcsWithOneStepAndResolveThatRaisesAnEFidoTestExceptionSetStatusToFailed;
var
  Result: TAsyncProcStatus;
begin
  Result := AsyncProcs.
    Queue(
      procedure
      begin
        raise EFidoTestException.Create('Error Message');
      end).
    Run.
    Resolve;

  Assert.AreEqual(TAsyncProcstatus.Failed, Result);
end;

procedure TAsyncProcsTests.AsyncProcsWithMultipleStepsAndResolveThatRaisesAnEFidoTestExceptionSetStatusToFailed;
var
  Result: TAsyncProcStatus;
begin
  Result := AsyncProcs.
    Queue(
      procedure
      begin
        sleep(10);
      end).
    &Then(
      procedure
      begin
        raise EFidoTestException.Create('Error Message');
      end).
    Run.
    Resolve;

  Assert.AreEqual(TAsyncProcstatus.Failed, Result);
end;

procedure TAsyncProcsTests.AsyncProcsWithOneStepAndManagedCatchAndResolveThatRaisesAnEFidoTestExceptionSetStatusToFinished;
var
  Result: TAsyncProcStatus;
begin
  Result := AsyncProcs.
    Queue(
      procedure
      begin
        raise EFidoTestException.Create('Error Message');
      end).
    Catch(
      procedure(const E: Exception)
      begin
      end).
    Run.
    Resolve;

  Assert.AreEqual(TAsyncProcstatus.Finished, Result);
end;

procedure TAsyncProcsTests.AsyncProcsWithMultipleStepsAndManagedCatchAndResolveThatRaisesAnEFidoTestExceptionSetStatusToFinished;
var
  Result: TAsyncProcStatus;
begin
  Result := AsyncProcs.
    Queue(
      procedure
      begin
        Sleep(10);
      end).
    &Then(
      procedure
      begin
        raise EFidoTestException.Create('Error Message');
      end).
    Catch(
      procedure(const E: Exception)
      begin
      end).
    Run.
    Resolve;

  Assert.AreEqual(TAsyncProcstatus.Finished, Result);
end;

procedure TAsyncProcsTests.AsyncProcsWithOneStepAndResolveSetsStatusToCancelledWhenItExpires;
var
  Result: TAsyncProcStatus;
begin
  Result := AsyncProcs.
    Queue(
      procedure
      begin
        Sleep(50);
      end).
    Within(
      10,
      procedure
      begin
      end).
    Run.
    Resolve;

  Assert.AreEqual(TAsyncProcstatus.Expired, Result);
end;

procedure TAsyncProcsTests.AsyncProcsWithMultipleStepsAndResolveSetsStatusToCancelledWhenItExpires;
var
  Result: TAsyncProcStatus;
begin
  Result := AsyncProcs.
    Queue(
      procedure
      begin
        Sleep(5);
      end).
    &Then(
      procedure
      begin
        Sleep(50);
      end).
    Within(
      10,
      procedure
      begin
      end).
    Run.
    Resolve;

  Assert.AreEqual(TAsyncProcstatus.Expired, Result);
end;

initialization
  TDUnitX.RegisterTestFixture(TAsyncProcsTests);
end.
