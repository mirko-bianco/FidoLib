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

unit Fido.Api.Client.VirtualApi.Attributes;

interface

uses
  System.SysUtils,

  Fido.Http.Types;

type
  // Base attribute for Virtual Apis
  ClientVirtualApiAttribute = class abstract(TCustomAttribute);

  // forces certain API version via "accept" header (e.g. Accept = "application/json;version=1.2.3")
  ApiVersionAttribute = class(ClientVirtualApiAttribute)
  private
    FVersion: string;
  public
    constructor Create(const Version: string);

    property Version: string read FVersion;
  end;

  // Disables redirects for specific endpoint (fixes mishandling of 304s)
  DisableRedirectsAttribute = class(ClientVirtualApiAttribute);

  // Describes the method and the endpoint path of an Api call
  // [Endpoint(rmPUT, '/account/{accountId}')]
  EndpointAttribute = class(ClientVirtualApiAttribute)
  private
    FMethod: THttpMethod;
    FEndPoint: string;
  public
    constructor Create(const Method: THttpMethod; const EndPoint: string);

    property EndPoint: string read FEndPoint;
    property Method: THttpMethod read FMethod;
  end;

  // Base attribute for params
  ParamAttribute = class abstract(ClientVirtualApiAttribute)
  private
    FMethodParam: string;
    FApiParam: string;
  public
    constructor Create(const MethodParam: string; const ApiParam: string = '');

    property MethodParam: string read FMethodParam;
    property ApiParam: string read FApiParam;
  end;

  // To be used to override the content of the request/response.
  // By default the value is 'application/json'
  ContentAttribute = class(ClientVirtualApiAttribute)
  private
    FContent: string;
  public
    constructor Create(const Content: string);

    property Content: string read FContent;
  end;

  // Following attributes describe the Query, Header, Form and File parameter names
  // [HeaderParam('ApiKey', 'Api-Key')]
  // [HeaderParam('ApiKey')]
  // The first parameter is the internal name, i.e. the one that will be searched in the RTTI of configuration functions and Api call parameters
  QueryParamAttribute = class(ParamAttribute);
  HeaderParamAttribute = class(ParamAttribute);
  FormParamAttribute = class(ParamAttribute);
  FileParamAttribute = class(ParamAttribute);

  // The parameter the eventual body parameter. If present, this parameter (that must be a class) will be converted to a Json and used as body of the call
  // i.e:
  // [RequestParam('Request')]
  // function AccountByAccountIdPut([ApiParam('AccountId')] Account_Id: Integer; Request: TUpdateIdentityRequest): TUpdateIdentityResponse;
  RequestParamAttribute = class(ParamAttribute);

  // Same as RequestParam, but the value is not converted to a Json value
  RawRequestParamAttribute = class(ParamAttribute);

  // This attribute indicates the Api name of a parameter and can be attached to an Api method parameter or a Configuration function
  // i.e.:
  // IIdentityApi = interface(IClientVirtualApi)
  //   ['{6825ED01-3041-4528-8CA2-993DDBBCB9A7}']
  //   ...
  //   function AccountByAccountIdPut([ApiParam('AccountId')] Account_Id: Integer; Request: TUpdateIdentityRequest): TUpdateIdentityResponse;
  // end;
  //
  // IMySettings = interface(IClientVirtualApiConfiguration)
  //   ['{D875BB18-F217-4208-8B41-17A867BA9F2B}']
  //   ...
  //   [ApiParam('Api-Key')]
  //   function GetApiKey: string;
  // end;
  ApiParamAttribute = class(ClientVirtualApiAttribute)
  private
    FParamName: string;
  public
    constructor Create(const ParamName: string);

    property ParamName: string read FParamName;
  end;

  ResponseHeaderParamAttribute = class(ClientVirtualApiAttribute)
  private
    FResponseCode: Integer;
    FHeaderParam: string;
    FParamName: string;
  public
    constructor Create(const ResponseCode: Integer; const HeaderParam: string; const ParamName: string = '');

    property ResponseCode: Integer read FResponseCode;
    property HeaderParam: string read FHeaderParam;
    property ParamName: string read FParamName;
  end;

  ConvertResponseForErrorCodeAttribute = class(ClientVirtualApiAttribute)
  private
    FErrorCode: integer;
    FParamName: string;
  public
    constructor Create(const ErrorCode: integer; const ParamName: string);

    property ErrorCode: integer read FErrorCode;
    property ParamName: string read FParamName;
  end;

implementation

{ ApiVersionAttribute }

constructor ApiVersionAttribute.Create(const Version: string);
begin
  inherited Create;
  FVersion := Version;
end;

{ EndpointAttribute }

constructor EndpointAttribute.Create(
  const Method: THttpMethod;
  const EndPoint: string);
begin
  inherited Create;
  FMethod := Method;
  FEndPoint := EndPoint;
end;

{ ParamsAttribute }

constructor ParamAttribute.Create(
  const MethodParam: string;
  const ApiParam: string);
begin
  inherited Create;
  FMethodParam := MethodParam;
  FApiParam := ApiParam;
  if FApiParam.IsEmpty then
    FApiParam := FMethodParam;
end;

{ ApiParamAttribute }

constructor ApiParamAttribute.Create(const ParamName: string);
begin
  FParamName := ParamName;
end;

{ ContentAttribute }

constructor ContentAttribute.Create(const Content: string);
begin
  FContent := Content;
end;

{ ResponseHeaderParamAttribute }

constructor ResponseHeaderParamAttribute.Create(
  const ResponseCode: Integer;
  const HeaderParam, ParamName: string);
begin
  inherited Create;
  FResponseCode := ResponseCode;
  FHeaderParam := HeaderParam;
  FParamName := ParamName;
  if FParamName.IsEmpty then
    FParamName := HeaderParam;
end;

{ ConvertResponseForErrorCodeAttribute }

constructor ConvertResponseForErrorCodeAttribute.Create(
  const ErrorCode: integer;
  const ParamName: string);
begin
  inherited Create;
  FErrorCode := ErrorCode;
  FParamName := ParamName;
end;

end.
