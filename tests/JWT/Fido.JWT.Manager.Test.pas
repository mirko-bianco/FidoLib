unit Fido.JWT.Manager.Test;

interface

uses
  System.SysUtils,
  System.JSON,

  DUnitX.TestFramework,

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
    procedure JWTManagerGeneratesACorrectTokenWithoutExpiration;

    [Test]
    procedure JWTManagerSingsTokenAndReturnsCorrectly;

    [Test]
    procedure JWTManagerVerifiesAValidTokenCorrectly;

    [Test]
    procedure JWTManagerDoesNotVerifyAnInvalidToken;

    [Test]
    procedure JWTManagerDeserializesAValidTokenCorrectly;

    [Test]
    procedure JWTManagerDoesNotDeserializeAnInvalidToken;
  end;

implementation

{ TJWTManagerTests }

procedure TJWTManagerTests.JWTManagerDoesNotDeserializeAnInvalidToken;
var
  Manager: Shared<TJWTManager>;
  Token: TJWT;
begin
  Token := Manager.Value.DeserializeToken(MockUtils.SomeString);

  Assert.IsNull(Token);
end;

procedure TJWTManagerTests.JWTManagerDoesNotVerifyAnInvalidToken;
var
  Manager: Shared<TJWTManager>;
  VerifiedToken: TJWT;
begin
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

procedure TJWTManagerTests.JWTManagerGeneratesACorrectTokenWithoutExpiration;
var
  Manager: Shared<TJWTManager>;
  Token: Shared<TJWT>;
  Issuer: string;
begin
  Issuer := MockUtils.SomeString;
  Manager := TJWTManager.Create;

  Token := Manager.Value.GenerateToken(Issuer);

  Sleep(100);

  Assert.AreEqual<TDateTime>(0, Token.Value.Claims.Expiration);
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

procedure TJWTManagerTests.JWTManagerDeserializesAValidTokenCorrectly;
var
  Manager: Shared<TJWTManager>;
  Token: Shared<TJWT>;
  Duration: Integer;
  Issuer: string;
  SignedToken: string;
  DeserializedToken: TJWT;
  Secret: string;
begin
  Duration := MockUtils.SomeInteger;
  Issuer := MockUtils.SomeString;
  Manager := TJWTManager.Create;

  Token := Manager.Value.GenerateToken(Issuer, Duration);
  Secret := MockUtils.SomeString;
  SignedToken := Manager.Value.SignTokenAndReturn(Token, TJOSEAlgorithmId.HS256, Secret, Secret);

  DeserializedToken := Manager.Value.DeserializeToken(SignedToken);
  try

    Assert.IsNotNull(DeserializedToken);
    Assert.AreEqual(Token.Value.Claims.JSON.ToJSON, DeserializedToken.Claims.JSON.ToJSON);

  finally
    DeserializedToken.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TJWTManagerTests);
end.
