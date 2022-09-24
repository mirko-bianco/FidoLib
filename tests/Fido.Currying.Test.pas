unit Fido.Currying.Test;

interface

uses
  DUnitX.TestFramework,
  SysUtils,

  Fido.Types,
  Fido.Currying;

type
  [TestFixture]
  TCurryingTests = class
  public
    [Test]
    procedure TwoParamsCurryingWorks;

    [Test]
    procedure ThreeParamsCurryingWorks;

    [Test]
    procedure FourParamsCurryingWorks;
  end;

implementation

procedure TCurryingTests.FourParamsCurryingWorks;
var
  TheFunc: TFourParamsFunction<string, string, string, string, string>;
begin
  TheFunc := function(const P1: string; const P2: string; const P3: string; const P4: string): string
    begin
      Result := P1 + P2 + P3 + P4;
    end;

  Assert.AreEqual<string>('ABCDEFGH', Curry.Cook<string, string, string, string, string>(TheFunc)('AB')('CD')('EF')('GH'));
end;

procedure TCurryingTests.ThreeParamsCurryingWorks;
var
  TheFunc: TThreeParamsFunction<string, string, string, string>;
begin
  TheFunc := function(const P1: string; const P2: string; const P3: string): string
    begin
      Result := P1 + P2 + P3;
    end;

  Assert.AreEqual<string>('ABCDEF', Curry.Cook<string, string, string, string>(TheFunc)('AB')('CD')('EF'));
end;

procedure TCurryingTests.TwoParamsCurryingWorks;
var
  TheFunc: TTwoParamsFunction<string, string, string>;
begin
  TheFunc := function(const P1: string; const P2: string): string
    begin
      Result := P1 + P2;
    end;

  Assert.AreEqual<string>('ABCDEF', Curry.Cook<string, string, string>(TheFunc)('ABC')('DEF'));
end;

initialization
  TDUnitX.RegisterTestFixture(TCurryingTests);
end.
