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

unit Fido.Api.Client.VirtualApi.Call;

interface

uses
  System.Classes,
  Rest.Types,

  Spring,
  Spring.Collections;

type
  TClientVirtualApiCallParameterKind = (pkPath, pkQuery, pkHeader, pkForm, pkFile);

  TClientVirtualApiCallParameter = record
    Kind: TClientVirtualApiCallParameterKind;
    Name: String;
    Value: String;
  end;

  TClientVirtualApiCall = class
  public const
    HTTP_OK_MINIMUN_RANGE = 200;
    HTTP_OK_MAXIMUN_RANGE = 299;
    DEFAULT_TIMEOUT_IN_MSECS = 5000;
  private
    FApiName: String;
    FApiMethodName: String;
    FApiMethod: TRESTRequestMethod;
    FStarted: TDateTime;
    FFinished: TDateTime;
    FResponseCode: Integer;
    FResponseContent: String;
    FResponseHeaders: TStrings;
    FParameters: IList<TClientVirtualApiCallParameter>;
    function GetIsOK: Boolean;
    function GetDurationInMsecs: Cardinal;
    function GetParameters: IReadOnlyList<TClientVirtualApiCallParameter>;
    procedure SetApiMethodName(const Value: String);
    procedure SetApiName(const Value: String);
  public
    Url: string;
    ContentType: string;
    PostBody: string;
    HandleRedirects: boolean;
    Timeout: Cardinal;
  public
    constructor Create(
      const ApiName,
            ApiMethodName: String;
      const ApiMethod: TRESTRequestMethod);
    destructor Destroy; override;

    function TryGetParameterValue(
      const Kind: TClientVirtualApiCallParameterKind;
      const Name: String;
      out Value: String): Boolean;
    procedure SetParameter(
      const Kind: TClientVirtualApiCallParameterKind;
      const Name,
            Value: String);

    procedure Start;
    procedure Finish(
      const ResponseCode: Integer;
      const ResponseContent: String;
      const ResponseHeaders: TStrings = nil);

    property ApiName: String read FApiName write SetApiName;
    property ApiMethodName: String read FApiMethodName write SetApiMethodName;
    property ApiMethod: TRESTRequestMethod read FApiMethod;
    property Parameters: IReadOnlyList<TClientVirtualApiCallParameter> read GetParameters;
    property IsOK: Boolean read GetIsOK;
    property Started: TDateTime read FStarted;
    property Ended: TDateTime read FStarted;
    property DurationInMsecs: Cardinal read GetDurationInMsecs;
    property ResponseCode: Integer read FResponseCode;
    property ResponseContent: String read FResponseContent;
    property ResponseHeaders: TStrings read FResponseHeaders;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils;

{ TClientVirtualApiCall }

constructor TClientVirtualApiCall.Create(const ApiName, ApiMethodName: String; const ApiMethod: TRESTRequestMethod);
begin
  Guard.CheckFalse(ApiName.IsEmpty);
  Guard.CheckFalse(ApiMethodName.IsEmpty);
  inherited Create;
  Self.ApiName := ApiName;
  Self.ApiMethodName := ApiMethodName;
  FApiMethod := ApiMethod;
  FParameters := TCollections.CreateList<TClientVirtualApiCallParameter>;
  FResponseHeaders := TStringList.Create;
  Timeout := DEFAULT_TIMEOUT_IN_MSECS;
end;

destructor TClientVirtualApiCall.Destroy;
begin
  FResponseHeaders.Free;
  inherited;
end;

procedure TClientVirtualApiCall.Finish(const ResponseCode: Integer;
  const ResponseContent: String; const ResponseHeaders: TStrings);
begin
  FFinished := Now;
  FResponseCode := ResponseCode;
  FResponseContent := ResponseContent;
  FResponseHeaders.Clear;
  if Assigned(ResponseHeaders) then
    FResponseHeaders.AddStrings(ResponseHeaders);
end;

function TClientVirtualApiCall.GetDurationInMsecs: Cardinal;
begin
  Result := MilliSecondsBetween(FFinished, FStarted);
end;

function TClientVirtualApiCall.GetIsOK: Boolean;
begin
  Result := (ResponseCode >= HTTP_OK_MINIMUN_RANGE) and (ResponseCode <= HTTP_OK_MAXIMUN_RANGE);
end;

function TClientVirtualApiCall.GetParameters: IReadOnlyList<TClientVirtualApiCallParameter>;
begin
  Result := FParameters.AsReadOnlyList;
end;

procedure TClientVirtualApiCall.SetApiMethodName(const Value: String);
begin
  Guard.CheckFalse(Value.IsEmpty);
  FApiMethodName := Value;
end;

procedure TClientVirtualApiCall.SetApiName(const Value: String);
begin
  Guard.CheckFalse(Value.IsEmpty);
  FApiName := Value;
end;

procedure TClientVirtualApiCall.SetParameter(const Kind: TClientVirtualApiCallParameterKind;
  const Name, Value: String);
var
  Index: Integer;
  Parameter: TClientVirtualApiCallParameter;
begin
  if Name.IsEmpty then
    Exit;

  for Index := 0 to FParameters.Count - 1 do
  begin
    Parameter := FParameters[Index];
    if (Parameter.Kind = Kind) and (Parameter.Name = Name) then
    begin
      if Value.IsEmpty then
        FParameters.Delete(Index)
      else
      begin
        Parameter.Value := Value;
        FParameters[Index] := Parameter;
      end;

      Exit;
    end;
  end;

  // no empties, sorry
  if Value.IsEmpty then
    Exit;

  // not found, add
  Parameter.Kind := Kind;
  Parameter.Name := Name;
  Parameter.Value := Value;

  FParameters.Add(Parameter);
end;

procedure TClientVirtualApiCall.Start;
begin
  FStarted := Now;
end;

function TClientVirtualApiCall.TryGetParameterValue(const Kind: TClientVirtualApiCallParameterKind;
  const Name: String; out Value: String): Boolean;
var
  Parameter: TClientVirtualApiCallParameter;
begin
  for Parameter in FParameters do
    if (Parameter.Kind = Kind) and SameText(Name, Parameter.Name) then
    begin
      Result := True;
      Value := Parameter.Value;
      Exit;
    end;

  Result := false;
end;

end.
