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
  private
    FSecret: TJOSEBytes;
    FAlgorithm: TJOSEAlgorithmId;
    FIssuer: string;
    FDefaultValidityInSecs: Integer;
  public
    constructor Create(const Secret: TJOSEBytes; const Algorithm: TJOSEAlgorithmId; const Issuer: string; const DefaultValidityInSecs: Integer);

    function TryVerifyToken(const CompactToken: string; out Token: TJWT): Boolean;
    function GenerateToken: TJWT;
    function SignTokenAndReturn(const Token: TJWT): string;
    function ValidityInSecs: Integer;
  end;

implementation

{ TJWTManager }

constructor TJWTManager.Create(
  const Secret: TJOSEBytes;
  const Algorithm: TJOSEAlgorithmId;
  const Issuer: string;
  const DefaultValidityInSecs: Integer);
begin
  inherited Create;

  FSecret := Secret;
  FAlgorithm := Algorithm;
  FIssuer := Issuer;
  FDefaultValidityInSecs := DefaultValidityInSecs;
end;

function TJWTManager.TryVerifyToken(
  const CompactToken: string;
  out Token: TJWT): Boolean;
var
  Key: Shared<TJWK>;
begin
  Result := False;
  Key := TJWK.Create(FSecret);
  try
    Token := TJOSE.Verify(Key.Value, CompactToken);
    Result := Token.Verified;
  except
    Token := nil;
  end;
end;

function TJWTManager.ValidityInSecs: Integer;
begin
  Result := FDefaultValidityInSecs;
end;

function TJWTManager.GenerateToken: TJWT;
begin
  Result := TJWT.Create;
  Result.Claims.Issuer := FIssuer;
  Result.Claims.IssuedAt := Now;
  Result.Claims.Expiration := Result.Claims.IssuedAt + (FDefaultValidityInSecs / 60 / 60 / 24);
end;

function TJWTManager.SignTokenAndReturn(const Token: TJWT): string;
var
  Signer: Shared<TJWS>;
  Key: Shared<TJWK>;
begin
  Signer := TJWS.Create(Token);
  Key := TJWK.Create(FSecret);
  Signer.Value.SkipKeyValidation := True;
  Signer.Value.Sign(Key, FAlgorithm);
  Signer.Value.VerifySignature;

  Result := Signer.Value.CompactToken;
end;

end.
