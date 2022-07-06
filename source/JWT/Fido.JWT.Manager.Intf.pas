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

unit Fido.JWT.Manager.Intf;

interface

uses
  System.Math,

  JOSE.Types.Bytes,
  JOSE.Core.JWA,
  JOSE.Core.JWT;

type
  IJWTManager = interface(IInvokable)
    ['{9D78F47A-99D9-49D6-9E14-732980F1D54B}']

    function VerifyToken(const CompactToken: string; const Secret: TJOSEBytes): TJWT;

    function GenerateToken(const Issuer: string; const DefaultValidityInSecs: Extended = System.Math.Infinity): TJWT;

    function SignTokenAndReturn(const Token: TJWT; const Algorithm: TJOSEAlgorithmId; const SigningSecret: TJOSEBytes; const VerificationSecret: TJOSEBytes): string;
  end;

implementation

end.
