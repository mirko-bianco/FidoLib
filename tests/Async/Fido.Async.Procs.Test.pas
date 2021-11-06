unit Fido.Async.Procs.Test;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Rtti,

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
    procedure AsyncProcsWithOneStepAndResolveThatRaisesAnExceptionSetStatusToFailed;

    [Test]
    procedure AsyncProcsWithOneStepAndManagedCatchAndResolveThatRaisesAnExceptionSetStatusToFinished;

    [Test]
    procedure AsyncProcsWithMultipleStepsAndResolveWaitsAndFinishes;

    [Test]
    procedure AsyncProcsWithMultipleStepsAndResolveThatRaisesAnExceptionSetStatusToFailed;

    [Test]
    procedure AsyncProcsWithMultipleStepsAndManagedCatchAndResolveThatRaisesAnExceptionSetStatusToFinished;

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

procedure TAsyncProcsTests.AsyncProcsWithOneStepAndResolveThatRaisesAnExceptionSetStatusToFailed;
var
  Result: TAsyncProcStatus;
begin
  Result := AsyncProcs.
    Queue(
      procedure
      begin
        raise Exception.Create('Error Message');
      end).
    Run.
    Resolve;

  Assert.AreEqual(TAsyncProcstatus.Failed, Result);
end;

procedure TAsyncProcsTests.AsyncProcsWithMultipleStepsAndResolveThatRaisesAnExceptionSetStatusToFailed;
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
        raise Exception.Create('Error Message');
      end).
    Run.
    Resolve;

  Assert.AreEqual(TAsyncProcstatus.Failed, Result);
end;

procedure TAsyncProcsTests.AsyncProcsWithOneStepAndManagedCatchAndResolveThatRaisesAnExceptionSetStatusToFinished;
var
  Result: TAsyncProcStatus;
begin
  Result := AsyncProcs.
    Queue(
      procedure
      begin
        raise Exception.Create('Error Message');
      end).
    Catch(
      procedure(const E: Exception)
      begin
      end).
    Run.
    Resolve;

  Assert.AreEqual(TAsyncProcstatus.Finished, Result);
end;

procedure TAsyncProcsTests.AsyncProcsWithMultipleStepsAndManagedCatchAndResolveThatRaisesAnExceptionSetStatusToFinished;
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
        raise Exception.Create('Error Message');
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
