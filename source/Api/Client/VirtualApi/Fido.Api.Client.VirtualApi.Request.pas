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

{$I Jedi.inc}
unit Fido.Api.Client.VirtualApi.Request;

interface

uses
  System.Classes,
  Rest.Types,
  Rest.Client;

type
  TApiClientVirtualApiRequest = class(TRestRequest)
  private
    FUrlSegmentsApplied: Boolean;
  protected
    procedure DoBeforeExecute; override;
    procedure DoApplyURLSegments(const AParamList: TRestRequestParameterArray; var AURL: string); override;
  end;

implementation

{ TApiClientVirtualApiRequest }

procedure TApiClientVirtualApiRequest.DoApplyURLSegments(
  const AParamList: TRestRequestParameterArray;
  var AURL: string);
var
  Index: Integer;
begin
  if not FUrlSegmentsApplied then
  begin
    inherited;

    FUrlSegmentsApplied := True;
  end
  else
  begin
    // TApiRequest currently doesn't support query params in put/post methods.
    // As long as the Api library isn't updated (by Embarcadero), we'll
    // explicitly call DoPrepareQueryString here after building the URL segments.
    if Self.Method in [TRESTRequestMethod.rmPUT, TRESTRequestMethod.rmPOST] then
    begin
      // Process query params in the url
      DoPrepareQueryString(AParamList, {$IFDEF DELPHIX_SYDNEY_UP}ContentType(AParamList),{$ENDIF} AURL);

      // Remove query params (else it will be processed in the body too).
      for Index := Params.Count-1 downto 0 do
        if TRestRequestParameter(Params[Index]).Kind = pkGETorPOST then
          Params.Delete(Params[Index]);
    end;

    URLAlreadyEncoded := True;
  end;
end;

procedure TApiClientVirtualApiRequest.DoBeforeExecute;
begin
  inherited;

  FUrlSegmentsApplied := False;
  URLAlreadyEncoded := False;
end;

end.

