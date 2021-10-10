(*
 * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without Apiriction, including without limitation the rights
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

unit Fido.Api.Server.Resources.JWTAuthorization.Abstract;

interface

uses
  System.SysUtils,

  Spring,

  JOSE.Core.JWT,

  Fido.Http.Types,
  Fido.JWT.Manager.Intf,
  Fido.Api.Server.Resource.Attributes,
  Fido.Api.Server.Resources.JWT.Dto.Request,
  Fido.Api.Server.Resources.JWT.Types;

type
  {$M+}
  [BaseUrl('/authorization')]
  TAbstractJWTAutorization = class abstract(TObject)
  strict private var
    FTokensManager: IJWTManager;
  protected
    procedure Authenticate(const Username: string; const Password: string; const ClientId: string; var Token: TJWT); virtual; abstract;
  public
    constructor Create(const TokensManager: IJWTManager);

    [Path(rmGET, '/token')]
    procedure GetToken(const [QueryParam('grant_type')] GrantType: string; const [QueryParam('username')] Username: string; const [QueryParam('password')] Password: string;
      const [QueryParam('client_id')] ClientId: string; out [BodyParam] Result: TJWTRequest);
  end;

implementation

{ TAbstractJWTAutorization }

constructor TAbstractJWTAutorization.Create(const TokensManager: IJWTManager);
begin
  inherited Create;
  Guard.CheckNotNull(TokensManager, 'TokensManager');

  FTokensManager := TokensManager;
end;

procedure TAbstractJWTAutorization.GetToken(
  const GrantType: string;
  const Username: string;
  const Password: string;
  const ClientId: string;
  out Result: TJWTRequest);
var
  Token: TJWT;
begin
  if SJWTSupportedGrantTypes[sgtPassword].ToUpper <> GrantType.ToUpper then
  begin
    Result := TJWTRequest.CreateFailure('grant_type is not supported.');
    Exit;
  end;
  Token := FTokensManager.GenerateToken;
  try
    try
      Authenticate(Username, Password, ClientId, Token);
      Result := TJWTRequest.CreateSuccess(FTokensManager.SignTokenAndReturn(Token), FTokensManager.ValidityInSecs);
    except
      on E: Exception do
        Result := TJWTRequest.CreateFailure(E.Message);
    end;
  finally
    Token.Free;
  end;
end;

end.
