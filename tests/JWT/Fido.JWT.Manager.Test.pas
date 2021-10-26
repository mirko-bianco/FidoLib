unit Fido.JWT.Manager.Test;

interface

uses
  DUnitX.TestFramework,
  SysUtils,
  System.JSON,

  Spring,

  JOSE.Types.Bytes,
  JOSE.Core.JWT,
  JOSE.Core.JWA,

  Fido.Testing.Mock.Utils,
  Fido.JWT.Manager;

type
  [TestFixture]
  TJWTManagerTests = class
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

procedure TJWTManagerTests.JWTManagerDoesNotVerifyAnInvalidToken;
var
  Manager: Shared<TJWTManager>;
  Issuer: string;
  VerifiedToken: TJWT;
begin
  Issuer := MockUtils.SomeString;

  VerifiedToken := Manager.Value.VerifyToken(MockUtils.SomeString, MockUtils.SomeString);

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
  Manager := TJWTManager.Create;

  Token := Manager.Value.GenerateToken(Issuer, Duration);

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
  Manager := TJWTManager.Create;

  Token := Manager.Value.GenerateToken(Issuer, Duration);

  Assert.WillNotRaiseAny(
    procedure
    var
      Secret: string;
    begin
      Secret := MockUtils.SomeString;
      SignedToken := Manager.Value.SignTokenAndReturn(Token, TJOSEAlgorithmId.HS256, Secret, Secret);
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
  Secret: string;
begin
  Duration := MockUtils.SomeInteger;
  Issuer := MockUtils.SomeString;
  Manager := TJWTManager.Create;

  Token := Manager.Value.GenerateToken(Issuer, Duration);
  Secret := MockUtils.SomeString;
  SignedToken := Manager.Value.SignTokenAndReturn(Token, TJOSEAlgorithmId.HS256, Secret, Secret);

  VerifiedToken := Manager.Value.VerifyToken(SignedToken, Secret);
  try

    Assert.IsNotNull(VerifiedToken);
    Assert.AreEqual(Token.Value.Verified, VerifiedToken.Verified);
    Assert.AreEqual(Token.Value.Claims.JSON.ToJSON, VerifiedToken.Claims.JSON.ToJSON);

  finally
    VerifiedToken.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TJWTManagerTests);
end.
