unit Fido.JWT.Manager.Test;

interface

uses
  DUnitX.TestFramework,
  SysUtils,

  Spring,

  JOSE.Types.Bytes,
  JOSE.Core.JWT,

  Fido.Testing.Mock.Utils,
  Fido.JWT.Manager;

type
  [TestFixture]
  TJWTManagerTests = class
    [Test]
    procedure JWTManagerReturnsValidityInSecs;

    [Test]
    procedure JWTManagerGeneratesACorrectToken;

    [Test]
    procedure JWTManagerSingsTokenAndReturnsCorrectly;

    [Test]
    procedure JWTManagerVerifiesAValidTokenCorrectly;

    [Test]
    procedure JWTManagerDoesNotVerifyAnInvalidToken;
  end;

implementation

{ TJWTManagerTests }

procedure TJWTManagerTests.JWTManagerReturnsValidityInSecs;
var
  Manager: Shared<TJWTManager>;
  Seconds: Integer;
begin
  Seconds := MockUtils.SomeInteger;

  Manager := TJWTManager.Create(MockUtils.SomeString, MockUtils.SomeString, Seconds);

  Assert.AreEqual(Seconds, Manager.Value.ValidityInSecs);
end;

procedure TJWTManagerTests.JWTManagerDoesNotVerifyAnInvalidToken;
var
  Manager: Shared<TJWTManager>;
  Token: Shared<TJWT>;
  Duration: Integer;
  Issuer: string;
  SignedToken: string;
  VerifiedToken: TJWT;
  Check: Boolean;
begin
  Duration := MockUtils.SomeInteger;
  Issuer := MockUtils.SomeString;
  Manager := TJWTManager.Create(MockUtils.SomeString, Issuer, Duration);

  Check := Manager.Value.TryVerifyToken(MockUtils.SomeString, VerifiedToken);

  Assert.IsFalse(Check);
  Assert.IsNull(VerifiedToken);
end;

procedure TJWTManagerTests.JWTManagerGeneratesACorrectToken;
var
  Manager: Shared<TJWTManager>;
  Token: Shared<TJWT>;
  Duration: Integer;
  Issuer: string;
begin
  Duration := MockUtils.SomeInteger;
  Issuer := MockUtils.SomeString;
  Manager := TJWTManager.Create(MockUtils.SomeString, Issuer, Duration);

  Token := Manager.Value.GenerateToken;

  Sleep(100);

  Assert.AreEqual<Real>(Trunc(Duration/60/60/24*10000), Trunc((Token.Value.Claims.Expiration - Token.Value.Claims.IssuedAt)*10000));
  Assert.AreEqual(Issuer, Token.Value.Claims.Issuer);
end;

procedure TJWTManagerTests.JWTManagerSingsTokenAndReturnsCorrectly;
var
  Manager: Shared<TJWTManager>;
  Token: Shared<TJWT>;
  Duration: Integer;
  Issuer: string;
  SignedToken: string;
begin
  Duration := MockUtils.SomeInteger;
  Issuer := MockUtils.SomeString;
  Manager := TJWTManager.Create(MockUtils.SomeString, Issuer, Duration);

  Token := Manager.Value.GenerateToken;

  Assert.WillNotRaiseAny(
    procedure
    begin
      SignedToken := Manager.Value.SignTokenAndReturn(Token);
    end
  );

  Assert.IsNotEmpty(SignedToken);
end;

procedure TJWTManagerTests.JWTManagerVerifiesAValidTokenCorrectly;
var
  Manager: Shared<TJWTManager>;
  Token: Shared<TJWT>;
  Duration: Integer;
  Issuer: string;
  SignedToken: string;
  VerifiedToken: TJWT;
  Check: Boolean;
begin
  Duration := MockUtils.SomeInteger;
  Issuer := MockUtils.SomeString;
  Manager := TJWTManager.Create(MockUtils.SomeString, Issuer, Duration);

  Token := Manager.Value.GenerateToken;
  SignedToken := Manager.Value.SignTokenAndReturn(Token);

  Check := Manager.Value.TryVerifyToken(SignedToken, VerifiedToken);
  try

    Assert.IsTrue(Check);
    Assert.AreEqual(Token.Value.Verified, VerifiedToken.Verified);
    Assert.AreEqual(Token.Value.Claims.JSON.ToJSON, VerifiedToken.Claims.JSON.ToJSON);

  finally
    VerifiedToken.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TJWTManagerTests);
end.
