unit Fido.Caching.test;

interface

uses
  DUnitX.TestFramework,
  SysUtils,

  Fido.Types,
  Fido.Caching.Intf,
  Fido.Caching.OneParam.FIFO,
  Fido.Caching.OneParam.Memoize,
  Fido.Caching.OneParam.Usage;

type
  [TestFixture]
  TCachingTests = class
  public
    [Test]
    procedure FIFOCachingWorks;

    [Test]
    procedure FIFOForceCachingWorks;

    [Test]
    procedure MemoizeWorks;

    [Test]
    procedure ForceMemoizeWorks;

    [Test]
    procedure UsageCachingWorks;

    [Test]
    procedure UsageForceCachingWorks;
  end;

implementation

var
  Count: Integer;

function Add1(const P: Integer): Integer;
begin
  Result := P + 1;
  Inc(Count);
end;

procedure TCachingTests.FIFOCachingWorks;
var
  Cache: IOneParamCache<Integer, Integer>;
  Result: Integer;
begin
  Count := 0;

  Cache := TFIFOOneParamCache<Integer, Integer>.Create(2);

  // Call with 1
  Result := Cache.It(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(1, Count);

  // Call with 1 again
  Result := Cache.It(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(1, Count);

  // Call with 2
  Result := Cache.It(Add1, 2);

  Assert.AreEqual(3, Result);
  Assert.AreEqual(2, Count);

  // Call with 2 again
  Result := Cache.It(Add1, 2);

  Assert.AreEqual(3, Result);
  Assert.AreEqual(2, Count);

  // Call with 3 (the 1 should be gone)
  Result := Cache.It(Add1, 3);

  Assert.AreEqual(4, Result);
  Assert.AreEqual(3, Count);

  // Call with 1, it should be calculated again
  Result := Cache.It(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(4, Count);
end;

procedure TCachingTests.FIFOForceCachingWorks;
var
  Cache: IOneParamCache<Integer, Integer>;
  Result: Integer;
begin
  Count := 0;

  Cache := TFIFOOneParamCache<Integer, Integer>.Create(2);

  // Call with 1
  Result := Cache.ForceIt(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(1, Count);

  // Call with 1 again
  Result := Cache.ForceIt(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(2, Count);

  // Call with 2
  Result := Cache.ForceIt(Add1, 2);

  Assert.AreEqual(3, Result);
  Assert.AreEqual(3, Count);

  // Call with 2 again
  Result := Cache.ForceIt(Add1, 2);

  Assert.AreEqual(3, Result);
  Assert.AreEqual(4, Count);

  // Call with 3
  Result := Cache.ForceIt(Add1, 3);

  Assert.AreEqual(4, Result);
  Assert.AreEqual(5, Count);

  // Call with 1
  Result := Cache.ForceIt(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(6, Count);
end;

procedure TCachingTests.ForceMemoizeWorks;
var
  Cache: IOneParamCache<Integer, Integer>;
  Result: Integer;
begin
  Count := 0;

  Cache := TMemoizeOneParam<Integer, Integer>.Create;

  // Call with 1
  Result := Cache.ForceIt(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(1, Count);

  // Call with 1 again
  Result := Cache.ForceIt(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(2, Count);

  // Call with 2
  Result := Cache.ForceIt(Add1, 2);

  Assert.AreEqual(3, Result);
  Assert.AreEqual(3, Count);

  // Call with 2 again
  Result := Cache.ForceIt(Add1, 2);

  Assert.AreEqual(3, Result);
  Assert.AreEqual(4, Count);

  // Call with 3
  Result := Cache.ForceIt(Add1, 3);

  Assert.AreEqual(4, Result);
  Assert.AreEqual(5, Count);

  // Call with 1
  Result := Cache.ForceIt(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(6, Count);
end;

procedure TCachingTests.MemoizeWorks;
var
  Cache: IOneParamCache<Integer, Integer>;
  Result: Integer;
begin
  Count := 0;

  Cache := TMemoizeOneParam<Integer, Integer>.Create;

  // Call with 1
  Result := Cache.It(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(1, Count);

  // Call with 1 again
  Result := Cache.It(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(1, Count);

  // Call with 2
  Result := Cache.It(Add1, 2);

  Assert.AreEqual(3, Result);
  Assert.AreEqual(2, Count);

  // Call with 2 again
  Result := Cache.It(Add1, 2);

  Assert.AreEqual(3, Result);
  Assert.AreEqual(2, Count);

  // Call with 3
  Result := Cache.It(Add1, 3);

  Assert.AreEqual(4, Result);
  Assert.AreEqual(3, Count);

  // Call with 1, it should still be there
  Result := Cache.It(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(3, Count);
end;

procedure TCachingTests.UsageCachingWorks;
var
  Cache: IOneParamCache<Integer, Integer>;
  Result: Integer;
begin
  Count := 0;

  Cache := TUsageOneParamCache<Integer, Integer>.Create(2);

  // Call with 1
  Result := Cache.It(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(1, Count);

  // Call with 1 again
  Result := Cache.It(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(1, Count);

  // Call with 2
  Result := Cache.It(Add1, 2);

  Assert.AreEqual(3, Result);
  Assert.AreEqual(2, Count);

  // Call with 2 again
  Result := Cache.It(Add1, 2);

  Assert.AreEqual(3, Result);
  Assert.AreEqual(2, Count);

  // Call with 1 again, now 2 has been used as last
  Result := Cache.It(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(2, Count);

  // Call with 3
  Result := Cache.It(Add1, 3);

  Assert.AreEqual(4, Result);
  Assert.AreEqual(3, Count);

  // Call with 1, it should still be there
  Result := Cache.It(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(3, Count);

  // Call with 2, it should not be cached anymore
  Result := Cache.It(Add1, 2);

  Assert.AreEqual(3, Result);
  Assert.AreEqual(4, Count);
end;

procedure TCachingTests.UsageForceCachingWorks;
var
  Cache: IOneParamCache<Integer, Integer>;
  Result: Integer;
begin
  Count := 0;

  Cache := TUsageOneParamCache<Integer, Integer>.Create(2);

  // Call with 1
  Result := Cache.ForceIt(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(1, Count);

  // Call with 1 again
  Result := Cache.ForceIt(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(2, Count);

  // Call with 2
  Result := Cache.ForceIt(Add1, 2);

  Assert.AreEqual(3, Result);
  Assert.AreEqual(3, Count);

  // Call with 2 again
  Result := Cache.ForceIt(Add1, 2);

  Assert.AreEqual(3, Result);
  Assert.AreEqual(4, Count);

  // Call with 1 again
  Result := Cache.ForceIt(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(5, Count);

  // Call with 3
  Result := Cache.ForceIt(Add1, 3);

  Assert.AreEqual(4, Result);
  Assert.AreEqual(6, Count);

  // Call with 1
  Result := Cache.ForceIt(Add1, 1);

  Assert.AreEqual(2, Result);
  Assert.AreEqual(7, Count);

  // Call with 2,
  Result := Cache.ForceIt(Add1, 2);

  Assert.AreEqual(3, Result);
  Assert.AreEqual(8, Count);
end;

initialization
  TDUnitX.RegisterTestFixture(TCachingTests);
end.
