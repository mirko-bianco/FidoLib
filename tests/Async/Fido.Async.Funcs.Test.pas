unit Fido.Async.Funcs.Test;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Rtti,

  Fido.Boxes,
  Fido.Async.Funcs;

type
  [TestFixture]
  TAsyncFuncsTests = class
  public
    [Test]
    procedure AsyncFuncMappingConvertsToActionOfTValueThatReturnsTheSameResult;

    [Test]
    procedure AsyncFuncsWithOneStepAndResolveWaitsFinishesAndReturnsTheCorrectValue;

    [Test]
    procedure AsyncFuncsWithOneStepAndResolveSetsStatusToCancelledWhenItExpires;

    [Test]
    procedure AsyncFuncsWithOneStepAndResolveThatRaisesAnExceptionSetStatusToFailed;

    [Test]
    procedure AsyncFuncsWithOneStepAndManagedCatchAndResolveThatRaisesAnExceptionSetStatusToFinishedAndReturnsTheManagedValue;

    [Test]
    procedure AsyncFuncsWithMultipleStepsAndResolveWaitsFinishesAndReturnsTheCorrectValue;

    [Test]
    procedure AsyncFuncsWithMultipleStepsAndResolveThatRaisesAnExceptionSetStatusToFailed;

    [Test]
    procedure AsyncFuncsWithMultipleStepsAndManagedCatchAndResolveThatRaisesAnExceptionSetStatusToFinishedAndReturnsTheManagedValue;

    [Test]
    procedure AsyncFuncsWithMultipleStepsAndResolveSetsStatusToCancelledWhenItExpires;
  end;

implementation

{ TAsyncFuncsTests }

procedure TAsyncFuncsTests.AsyncFuncMappingConvertsToActionOfTValueThatReturnsTheSameResult;
var
  Func: TAsyncFuncTypedAction<string, Integer>;
  ValueFunc: TAsyncFuncAction;
begin

  Func := function(const Value: string): Integer
  begin
    Result := StrToInt(Value);
  end;

  ValueFunc := AsyncFuncMapping.Action<string, Integer>(Func);

  Assert.AreEqual(Func('100'), ValueFunc(TValue.From<string>('100')).AsType<Integer>);
end;

procedure TAsyncFuncsTests.AsyncFuncsWithOneStepAndResolveWaitsFinishesAndReturnsTheCorrectValue;
var
  Result: TAsyncFuncResult<string>;
begin
  Result := AsyncFuncs<Integer, string>.
    Queue(
      AsyncFuncMapping.Action<Integer, string>(function(const Value: Integer): string
      begin
        Result := IntToStr(Value);
        Sleep(25);
      end)).
    Run(100).
    Resolve;

  Assert.AreEqual(TAsyncFuncStatus.Finished, Result.Status);
  Assert.AreEqual('100', Result.Value.Value);
end;

procedure TAsyncFuncsTests.AsyncFuncsWithMultipleStepsAndResolveWaitsFinishesAndReturnsTheCorrectValue;
var
  Result: TAsyncFuncResult<string>;
begin
  Result := AsyncFuncs<Integer, string>.
    Queue(
      AsyncFuncMapping.Action<Integer, integer>(function(const Value: Integer): Integer
      begin
        Result := Value + 100;
        Sleep(25);
      end)).
    &Then(AsyncFuncMapping.Action<Integer, string>(function(const Value: Integer): string
      begin
        Result := IntToStr(Value);
        Sleep(25);
      end)).
    Run(100).
    Resolve;

  Assert.AreEqual(TAsyncFuncStatus.Finished, Result.Status);
  Assert.AreEqual('200', Result.Value.Value);
end;

procedure TAsyncFuncsTests.AsyncFuncsWithOneStepAndResolveThatRaisesAnExceptionSetStatusToFailed;
var
  Result: TAsyncFuncResult<string>;
begin
  Result := AsyncFuncs<Integer, string>.
    Queue(
      AsyncFuncMapping.Action<Integer, string>(function(const Value: Integer): string
      begin
        raise Exception.Create('Error Message');
      end)).
    Run(100).
    Resolve;

  Assert.AreEqual(TAsyncFuncStatus.Failed, Result.Status);
  Assert.AreEqual(False, Result.Value.HasValue);
end;

procedure TAsyncFuncsTests.AsyncFuncsWithMultipleStepsAndResolveThatRaisesAnExceptionSetStatusToFailed;
var
  Result: TAsyncFuncResult<Integer>;
begin
  Result := AsyncFuncs<Integer, Integer>.
    Queue(
      AsyncFuncMapping.Action<Integer, Integer>(function(const Value: Integer): Integer
      begin
        Result := Value + 10;
      end)).
    &Then(AsyncFuncMapping.Action<Integer, Integer>(function(const Value: Integer): Integer
      begin
        raise Exception.Create('Error Message');
      end)).
    Run(100).
    Resolve;

  Assert.AreEqual(TAsyncFuncStatus.Failed, Result.Status);
  Assert.AreEqual(False, Result.Value.HasValue);
end;

procedure TAsyncFuncsTests.AsyncFuncsWithOneStepAndManagedCatchAndResolveThatRaisesAnExceptionSetStatusToFinishedAndReturnsTheManagedValue;
var
  Result: TAsyncFuncResult<string>;
begin
  Result := AsyncFuncs<Integer, string>.
    Queue(
      AsyncFuncMapping.Action<Integer, string>(function(const Value: Integer): string
      begin
        raise Exception.Create('Error Message');
      end)).
    Catch(
      function(const E: Exception): string
      begin
        Result := 'Managed result';
      end).
    Run(100).
    Resolve;

  Assert.AreEqual(TAsyncFuncStatus.Finished, Result.Status);
  Assert.AreEqual('Managed result', Result.Value.Value);
end;

procedure TAsyncFuncsTests.AsyncFuncsWithMultipleStepsAndManagedCatchAndResolveThatRaisesAnExceptionSetStatusToFinishedAndReturnsTheManagedValue;
var
  Result: TAsyncFuncResult<string>;
begin
  Result := AsyncFuncs<Integer, string>.
    Queue(
      AsyncFuncMapping.Action<Integer, Integer>(function(const Value: Integer): Integer
      begin
        Result := Value + 10;
      end)).
    &Then(
      AsyncFuncMapping.Action<Integer, Integer>(function(const Value: Integer): Integer
      begin
        raise Exception.Create('Error Message');
      end)).
    Catch(
      function(const E: Exception): string
      begin
        Result := 'Managed result';
      end).
    Run(100).
    Resolve;

  Assert.AreEqual(TAsyncFuncStatus.Finished, Result.Status);
  Assert.AreEqual('Managed result', Result.Value.Value);
end;

procedure TAsyncFuncsTests.AsyncFuncsWithOneStepAndResolveSetsStatusToCancelledWhenItExpires;
var
  Result: TAsyncFuncResult<string>;
begin
  Result := AsyncFuncs<Integer, string>.
    Queue(
      AsyncFuncMapping.Action<Integer, string>(function(const Value: Integer): string
      begin
        Result := IntToStr(Value);
        Sleep(50);
      end)).
    Within(
      10,
      function: string
      begin
        Result := 'Expired!';
      end).
    Run(100).
    Resolve;

  Assert.AreEqual(TAsyncFuncStatus.Expired, Result.Status);
  Assert.AreEqual('Expired!', Result.Value.Value);
end;

procedure TAsyncFuncsTests.AsyncFuncsWithMultipleStepsAndResolveSetsStatusToCancelledWhenItExpires;
var
  Result: TAsyncFuncResult<string>;
begin
  Result := AsyncFuncs<Integer, string>.
    Queue(
      AsyncFuncMapping.Action<Integer, Integer>(function(const Value: Integer): Integer
      begin
        Result := Value + 10;
        Sleep(5);
      end)).
    &Then(
      AsyncFuncMapping.Action<Integer, string>(function(const Value: Integer): string
      begin
        Result := IntToStr(Value);
        Sleep(50);
      end)).
    Within(
      10,
      function: string
      begin
        Result := 'Expired!';
      end).
    Run(100).
    Resolve;

  Assert.AreEqual(TAsyncFuncStatus.Expired, Result.Status);
  Assert.AreEqual('Expired!', Result.Value.Value);
end;

initialization
  TDUnitX.RegisterTestFixture(TAsyncFuncsTests);
end.
