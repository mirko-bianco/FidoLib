unit Fido.Currying.Test;

interface

uses
  DUnitX.TestFramework,
  SysUtils,

  Spring,

  Fido.Currying;

type
  [TestFixture]
  TCurryingTests = class
  public
    [Test]
    procedure OneParamFuncCurryingWorks;

    [Test]
    procedure TwoParamsFuncCurryingWorks;

    [Test]
    procedure ThreeParamsFuncCurryingWorks;

    [Test]
    procedure FourParamsFuncCurryingWorks;

    [Test]
    procedure OneParamProcCurryingWorks;

    [Test]
    procedure TwoParamsProcCurryingWorks;

    [Test]
    procedure ThreeParamsProcCurryingWorks;

    [Test]
    procedure FourParamsProcCurryingWorks;
  end;

implementation

procedure TCurryingTests.FourParamsFuncCurryingWorks;
var
  TheFunc: Func<string, string, string, string, string>;
begin
  TheFunc := function(const P1: string; const P2: string; const P3: string; const P4: string): string
    begin
      Result := P1 + P2 + P3 + P4;
    end;

  Assert.AreEqual<string>('ABCDEFGH', Curry.Cook<string, string, string, string, string>(TheFunc)('AB')('CD')('EF')('GH'));
end;

procedure TCurryingTests.ThreeParamsFuncCurryingWorks;
var
  TheFunc: Func<string, string, string, string>;
begin
  TheFunc := function(const P1: string; const P2: string; const P3: string): string
    begin
      Result := P1 + P2 + P3;
    end;

  Assert.AreEqual<string>('ABCDEF', Curry.Cook<string, string, string, string>(TheFunc)('AB')('CD')('EF'));
end;

procedure TCurryingTests.TwoParamsFuncCurryingWorks;
var
  TheFunc: Func<string, string, string>;
begin
  TheFunc := function(const P1: string; const P2: string): string
    begin
      Result := P1 + P2;
    end;

  Assert.AreEqual<string>('ABCDEF', Curry.Cook<string, string, string>(TheFunc)('ABC')('DEF'));
end;

procedure TCurryingTests.FourParamsProcCurryingWorks;
var
  TheProc: Action<string, string, string, string>;
  Result: string;
begin
  Result := '';

  TheProc := procedure(const P1: string; const P2: string; const P3: string; const P4: string)
    begin
      Result := P1 + P2 + P3 + P4;
    end;

  Curry.Cook<string, string, string, string>(TheProc)('AB')('CD')('EF')('GH');

  Assert.AreEqual<string>('ABCDEFGH', Result);
end;

procedure TCurryingTests.OneParamFuncCurryingWorks;
var
  TheFunc: Func<string, string>;
begin
  TheFunc := function(const P1: string): string
    begin
      Result := P1;
    end;

  Assert.AreEqual<string>('ABC', Curry.Cook<string, string>(TheFunc)('ABC')());
end;

procedure TCurryingTests.OneParamProcCurryingWorks;
var
  TheProc: Action<string>;
  Result: string;
begin
  Result := '';

  TheProc := procedure(const P1: string)
    begin
      Result := P1;
    end;

  Curry.Cook<string>(TheProc)('ABC')();

  Assert.AreEqual<string>('ABC', Result);
end;

procedure TCurryingTests.ThreeParamsProcCurryingWorks;
var
  TheProc: Action<string, string, string>;
  Result: string;
begin
  Result := '';

  TheProc := procedure(const P1: string; const P2: string; const P3: string)
    begin
      Result := P1 + P2 + P3;
    end;

  Curry.Cook<string, string, string>(TheProc)('AB')('CD')('EF');

  Assert.AreEqual<string>('ABCDEF', Result);
end;

procedure TCurryingTests.TwoParamsProcCurryingWorks;
var
  TheProc: Action<string, string>;
  Result: string;
begin
  Result := '';

  TheProc := procedure(const P1: string; const P2: string)
    begin
      Result := P1 + P2;
    end;

  Curry.Cook<string, string>(TheProc)('ABC')('DEF');

  Assert.AreEqual<string>('ABCDEF', Result);
end;

initialization
  TDUnitX.RegisterTestFixture(TCurryingTests);
end.
