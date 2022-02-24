(*
 * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

unit Fido.JWT.Manager;

interface

uses
  System.SysUtils,
  System.Math,

  JOSE.Types.Bytes,
  JOSE.Encoding.Base64,
  JOSE.Core.JWT,
  JOSE.Core.JWK,
  JOSE.Core.JWS,
  JOSE.Core.JWA,
  JOSE.Core.Builder,

  Spring,

  Fido.JWT.Manager.Intf;

type
  TJWTManager = class(TInterfacedObject, IJWTManager)
  public
    function VerifyToken(const CompactToken: string; const Secret: TJOSEBytes): TJWT;

    function GenerateToken(const Issuer: string; const DefaultValidityInSecs: Extended = System.Math.Infinity): TJWT;

    function SignTokenAndReturn(const Token: TJWT; const Algorithm: TJOSEAlgorithmId; const SigningSecret: TJOSEBytes; const VerificationSecret: TJOSEBytes): string;
  end;

implementation

{ TJWTManager }

function TJWTManager.VerifyToken(
  const CompactToken: string;
  const Secret: TJOSEBytes): TJWT;
var
  Key: Shared<TJWK>;
  Token: TJWT;
begin
  Result := nil;
  Key := TJWK.Create(Secret);
  try
    Token := TJOSE.Verify(Key.Value, CompactToken);
    if Token.Verified then
      Result := Token;
  except
  end;
end;

function TJWTManager.GenerateToken(
  const Issuer: string;
  const DefaultValidityInSecs: Extended): TJWT;
begin
  Result := TJWT.Create;
  Result.Claims.Issuer := Issuer;
  Result.Claims.IssuedAt := Now;
  if not IsInfinite(DefaultValidityInSecs) then
    Result.Claims.Expiration := Result.Claims.IssuedAt + (DefaultValidityInSecs / 60 / 60 / 24);
end;

function TJWTManager.SignTokenAndReturn(
  const Token: TJWT;
  const Algorithm: TJOSEAlgorithmId;
  const SigningSecret: TJOSEBytes;
  const VerificationSecret: TJOSEBytes): string;
var
  Signer: Shared<TJWS>;
  SigningKey: Shared<TJWK>;
  VerificationKey: Shared<TJWK>;
begin
  Signer := TJWS.Create(Token);
  SigningKey := TJWK.Create(SigningSecret);

  Signer.Value.SkipKeyValidation := False;
  Signer.Value.Sign(SigningKey, Algorithm);

  if VerificationSecret <> '' then
    VerificationKey := TJWK.Create(VerificationSecret)
  else
    VerificationKey := TJWK.Create(SigningSecret);

  Signer.Value.VerifySignature(VerificationKey, Signer.Value.CompactToken);

  Result := Signer.Value.CompactToken;
end;

end.
