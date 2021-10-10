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

unit Fido.Api.Server.Resources.JWT.Dto.Request;

interface

uses
  System.SysUtils,

  Spring,

  JOSE.Core.JWT,

  Fido.Http.Types,
  Fido.JWT.Manager.Intf,
  Fido.Api.Server.Resources.JWT.Types;

type
  TJWTRequest = class
  strict private
    FAccess_Token: Nullable<string>;
    FExpires_In: Nullable<Integer>;
    FError: Nullable<string>;
  public
    constructor CreateSuccess(const Access_Token: string; const Expires_In: Integer);
    constructor CreateFailure(const Error: string);

    property Access_Token: Nullable<string> read FAccess_Token;
    property Expires_In: Nullable<Integer> read FExpires_In;
    property Error: Nullable<string> read FError;
  end;

implementation

{ TJWTRequest }

constructor TJWTRequest.CreateFailure(const Error: string);
begin
  inherited Create;
  FError := Error;
end;

constructor TJWTRequest.CreateSuccess(
  const Access_Token: string;
  const Expires_In: Integer);
begin
  inherited Create;
  FAccess_Token := Access_Token;
  FExpires_In := Expires_In;
end;

end.
