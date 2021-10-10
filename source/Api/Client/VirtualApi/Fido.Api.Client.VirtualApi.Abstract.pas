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

unit Fido.Api.Client.VirtualApi.Abstract;

interface

uses
  System.DateUtils,
  System.StrUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  System.SysUtils,
  System.RegularExpressions,
  System.Generics.Defaults,
  System.Generics.Collections,
  Rest.Types,

  Spring,
  Spring.Collections,

  Fido.VirtualInterface,
  Fido.Api.Client.VirtualApi.Intf,
  Fido.Api.Client.VirtualApi.Attributes,
  Fido.Api.Client.VirtualApi.Configuration.Intf,
  Fido.Api.Client.Exception,
  Fido.Api.Client.VirtualApi.Call;

type
  TResponseHeaderParamInfo = record
  private
    FResponseCode: Integer;
    FHeaderParam: string;
    FParamName: string;
  public
    constructor Create(const ResponseCode: Integer; const HeaderParam: string; const ParamName: string);

    property ResponseCode: Integer read FResponseCode;
    property HeaderParam: string read FHeaderParam;
    property ParamName: string read FParamName;
  end;

  TConvertResponseForErrorCodeInfo = record
  strict private
    FErrorCode: Integer;
    FParamName: string;
  public
    constructor Create(const ErrorCode: Integer; const ParamName: string);

    property ErrorCode: integer read FErrorCode;
    property ParamName: string read FParamName;
  end;

  TClientVirtualApiEndPointInfo = record
  strict private
    FApiName: string;
    FMethod: TRESTRequestMethod;
    FEndPoint: string;
    // Method param => Api param. i.e. 'ApiKey' => 'Api-Key'
    FQueryParams: IDictionary<string, string>;
    FHeaderParams: IDictionary<string, string>;
    FFormParams: IDictionary<string, string>;
    FFileParams: IDictionary<string, string>;
    FRequestParam: string;
    FContent: string;
    FResponseHeaderParamInfo: Nullable<TResponseHeaderParamInfo>;
    FConvertResponseForErrorCodeInfo: Nullable<TConvertResponseForErrorCodeInfo>;
    FHandleRedirects: boolean;
  public
    constructor Create(const ApiName: string; const Method: TRESTRequestMethod; const EndPoint: string; const QueryParams: IDictionary<string, string>; const HeaderParams: IDictionary<string, string>;
      const FormParams: IDictionary<string, string>; const FileParams: IDictionary<string, string>; const RequestParam: string; const Content: string;
      const ResponseHeaderParamInfo: Nullable<TResponseHeaderParamInfo>; const ConvertResponseForErrorCodeInfo: Nullable<TConvertResponseForErrorCodeInfo>; const HandleRedirects: boolean);

    property ApiName: string read FApiName;
    property Method: TRESTRequestMethod read FMethod;
    property EndPoint: string read FEndPoint;
    property QueryParams: IDictionary<string, string> read FQueryParams;
    property HeaderParams: IDictionary<string, string> read FHeaderParams;
    property FormParams: IDictionary<string, string> read FFormParams;
    property FileParams: IDictionary<string, string> read FFileParams;
    property RequestParam: string read FRequestParam;
    property Content: string read FContent;
    property ResponseHeaderParamInfo: Nullable<TResponseHeaderParamInfo> read FResponseHeaderParamInfo;
    property ConvertResponseForErrorCodeInfo: Nullable<TConvertResponseForErrorCodeInfo> read FConvertResponseForErrorCodeInfo;
    property HandleRedirects: boolean read FHandleRedirects;
  end;

  TClientVirtualApiMisconfiguration = record
  strict private
    FApiName: string;
    FApiMethod: string;
    FParameterName: string;
  public
    constructor Create(const ApiName: string; const ApiMethod: string; const ParameterName: string);

    property ApiName: string read FApiName;
    property ApiMethod: string read FApiMethod;
    property ParameterName: string read FParameterName;
  end;

  {$M+}
  TAbstractClientVirtualApi<T: IClientVirtualApi; IConfiguration: IClientVirtualApiConfiguration> = class(TVirtualInterface<T>)
  strict private const
    MUSTACHE_REGEXPR = '{\s*[\w\.\-]+\s*}';
    FORMAT_PARAM = '{format}';
    DEFAULT_FORMAT = 'json';
  strict private class var
    FEndPointInfo: IDictionary<string, TClientVirtualApiEndPointInfo>;
    FMisconfigurations: IList<TClientVirtualApiMisconfiguration>;
    FShortApiName: string;
    FApiVersion: string;
  strict private var
    FConfiguration: IConfiguration;
    FLastStatusCode: integer;
  strict private
    class function InspectMethod(const Method: TRttiMethod): TClientVirtualApiEndPointInfo;
    class procedure InspectInterface(const ApiInterface: TRttiType);
    class function CheckParameterCoverage(const ParamName: string; const MethodParams: IDictionary<string, string>; const ConfigurationParams: IDictionary<string, string>): Boolean;
    class function CheckParametersCoverage(const ApiName: string; const MethodName: string; const Params: IDictionary<string, string>; const MethodParams: IDictionary<string, string>;
      const ConfigurationParams: IDictionary<string, string>; out Misconfigurations: TArray<TClientVirtualApiMisconfiguration>): Boolean;
    class function CheckParameterAgainstDictionary(const ParamName: string; const Dictionary: IDictionary<string, string>): Boolean;
    class procedure ValidateMethods;
    class procedure FindApiParamAttribute(const MethodName: string; const Attributes: TArray<TCustomAttribute>; const ConfigurationParams: IDictionary<string, string>); static;
    function ProcessPath(const Call: TClientVirtualApiCall; const EndPoint: string; const Arguments: IDictionary<string, TPair<string, TValue>>): string;
    procedure MapArgumentsAndParameters(const Parameters: TArray<TRttiParameter>; const Args: TArray<TValue>; const Arguments: IDictionary<string, TPair<string, TValue>>);
    procedure ParamNamesToParamNameValues(const Arguments: IDictionary<string, TPair<string, TValue>>; const Params: IDictionary<string, string>; const Kind: TClientVirtualApiCallParameterKind;
      const Call: TClientVirtualApiCall);
    procedure MapArgumentAndConfiguration(const Arguments: IDictionary<string, TPair<string, TValue>>);
    procedure DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
  protected const
    CONTENT = 'application/json';
  protected
    class function GetAcceptHeaderWithApiVersion(const AcceptHeader: string): string;
    function ConvertTValueToString(const Value: TValue): string; virtual; abstract;
    function ConvertResponseToDto(const Response: string; const TypeInfo: PTypeInfo): TValue; virtual; abstract;
    function ConvertRequestDtoToString(const Value: TValue): string; virtual; abstract;
    procedure CallApi(const Call: TClientVirtualApiCall); virtual; abstract;

    class property ApiVersion: string read FApiVersion;
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create(const Configuration: IConfiguration); overload;

    function IsActive: Boolean;
  end;

implementation

{ TAbstractClientVirtualApi<T> }

class constructor TAbstractClientVirtualApi<T, IConfiguration>.Create;
begin
  inherited;
  FEndPointInfo := TCollections.CreateDictionary<string, TClientVirtualApiEndPointInfo>(TIStringComparer.Ordinal);
  FMisconfigurations := TCollections.CreateList<TClientVirtualApiMisconfiguration>;
  ValidateMethods;
end;

class destructor TAbstractClientVirtualApi<T, IConfiguration>.Destroy;
begin
  FEndPointInfo := nil;
  inherited;
end;

class function TAbstractClientVirtualApi<T, IConfiguration>.CheckParameterAgainstDictionary(
  const ParamName: string;
  const Dictionary: IDictionary<string, string>): Boolean;
var
  ApiName: string;
begin
  Result := False;
  with Dictionary.Keys.GetEnumerator do
    while MoveNext do
    begin
      if SameText(Current, ParamName) or
         (Dictionary.TryGetValue(Current, ApiName) and
          SameText(ApiName, ParamName)) then
        Exit(True);
    end;
end;

class function TAbstractClientVirtualApi<T, IConfiguration>.CheckParameterCoverage(
  const ParamName: string;
  const MethodParams: IDictionary<string, string>;
  const ConfigurationParams: IDictionary<string, string>): Boolean;
var
  ApiName: string;
begin
  Result := CheckParameterAgainstDictionary(ParamName, MethodParams);
  if not Result then
    Result := CheckParameterAgainstDictionary(ParamName, ConfigurationParams);
end;

class function TAbstractClientVirtualApi<T, IConfiguration>.CheckParametersCoverage(
  const ApiName: string;
  const MethodName: string;
  const Params: IDictionary<string, string>;
  const MethodParams: IDictionary<string, string>;
  const ConfigurationParams: IDictionary<string, string>;
  out Misconfigurations: TArray<TClientVirtualApiMisconfiguration>): Boolean;
begin
  Result := True;
  SetLength(Misconfigurations, 0);
  with Params.Keys.GetEnumerator do
    while MoveNext do
      if not CheckParameterCoverage(Current, MethodParams, ConfigurationParams) then
      begin
        Result := False;
        SetLength(Misconfigurations, Length(Misconfigurations) + 1);
        Misconfigurations[High(Misconfigurations)] := TClientVirtualApiMisconfiguration.Create(ApiName, MethodName, Current);
      end;
end;

class function TAbstractClientVirtualApi<T, IConfiguration>.GetAcceptHeaderWithApiVersion(const AcceptHeader: string): string;
begin
  Result := AcceptHeader;
  if not ApiVersion.IsEmpty then
    Result := Result + ';version=' + ApiVersion;
end;

class procedure TAbstractClientVirtualApi<T, IConfiguration>.FindApiParamAttribute(
  const MethodName: string;
  const Attributes: TArray<TCustomAttribute>;
  const ConfigurationParams: IDictionary<string, string>);
var
  ApiName: string;
  RttiAttribute: TCustomAttribute;
begin
  ApiName := '';
  for RttiAttribute in Attributes do
    if RttiAttribute is ApiParamAttribute then
    begin
      ApiName := ApiParamAttribute(RttiAttribute).ParamName;
      Break
    end;

  ConfigurationParams.AddOrSetValue(MethodName, ApiName);
end;

class procedure TAbstractClientVirtualApi<T, IConfiguration>.ValidateMethods;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
  RttiAttribute: TCustomAttribute;
  MethodName: string;
  ApiName: string;
  ConfigurationParams: IDictionary<string, string>;
  MethodParams: IDictionary<string, string>;
  RttiParameter: TRttiParameter;
  EndPointInfo: TClientVirtualApiEndPointInfo;
  EndPoint: string;
  ParamName: string;
  Misconfiguration: TClientVirtualApiMisconfiguration;
  Misconfigurations: TArray<TClientVirtualApiMisconfiguration>;
  ResponseHeaderParamFound: Boolean;
  ConvertResponseForErrorCodeFound: boolean;
begin
  ConfigurationParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);
  MethodParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);

  // Get configuration extra available parameters
  RttiType := Context.GetType(TypeInfo(IConfiguration));
  if Assigned(RttiType) then
    for RttiMethod in RttiType.GetMethods do
      if (RttiMethod.MethodKind = mkFunction) and
         (Length(RttiMethod.GetParameters) = 0) then
      begin
        MethodName := RttiMethod.Name;
        if MethodName.ToUpper.StartsWith('GET') then
          MethodName := Copy(MethodName, 4, Length(MethodName));
          FindApiParamAttribute(MethodName, RttiMethod.GetAttributes, ConfigurationParams);
      end;

  // Validate the methods
  RttiType := Context.GetType(TypeInfo(T));
  if Assigned(RttiType) then
  begin
    FShortApiName := RttiType.Name;

    InspectInterface(RttiType);

    for RttiMethod in RttiType.GetMethods do
    begin
      EndPointInfo := InspectMethod(RttiMethod);

      // Retrieve exposed parameters
      for RttiParameter in RttiMethod.GetParameters do
        FindApiParamAttribute(RttiParameter.Name, RttiMethod.GetAttributes, MethodParams);

      // Validate endpoint parameters
      EndPoint := StringReplace(EndPointInfo.EndPoint, FORMAT_PARAM, DEFAULT_FORMAT, [rfReplaceAll]);
      with TRegEx.Create(MUSTACHE_REGEXPR, [roIgnoreCase, roMultiline]).Matches(EndPoint).GetEnumerator do
      try
        while MoveNext do
        begin
          ParamName := StringReplace(StringReplace(Current.Value, '{', '', [rfReplaceAll]), '}', '', [rfReplaceAll]);
          if not CheckParameterCoverage(ParamName, MethodParams, ConfigurationParams) then
            FMisconfigurations.Add(TClientVirtualApiMisconfiguration.Create(RttiType.QualifiedName, RttiMethod.Name, Current.Value));
        end;
      finally
        Free;
      end;

      // Validate request parameter
      if not CheckParameterCoverage(EndPointInfo.RequestParam, MethodParams, ConfigurationParams) then
        FMisconfigurations.Add(TClientVirtualApiMisconfiguration.Create(RttiType.QualifiedName, RttiMethod.Name, EndPointInfo.RequestParam));

      // Validate query parameters
      if not CheckParametersCoverage(RttiType.QualifiedName, RttiMethod.Name, EndPointInfo.QueryParams, MethodParams, ConfigurationParams, Misconfigurations) then
        FMisconfigurations.AddRange(Misconfigurations);

      // Validate header parameters
      if not CheckParametersCoverage(RttiType.QualifiedName, RttiMethod.Name, EndPointInfo.HeaderParams, MethodParams, ConfigurationParams, Misconfigurations) then
        FMisconfigurations.AddRange(Misconfigurations);

      // Validate form parameters
      if not CheckParametersCoverage(RttiType.QualifiedName, RttiMethod.Name, EndPointInfo.FormParams, MethodParams, ConfigurationParams, Misconfigurations) then
        FMisconfigurations.AddRange(Misconfigurations);

      // Validate file parameters
      if not CheckParametersCoverage(RttiType.QualifiedName, RttiMethod.Name, EndPointInfo.FileParams, MethodParams, ConfigurationParams, Misconfigurations) then
        FMisconfigurations.AddRange(Misconfigurations);

      if EndPointInfo.ResponseHeaderParamInfo.HasValue then
      begin
        ResponseHeaderParamFound := False;
        for RttiParameter in RttiMethod.GetParameters do
        begin
          ResponseHeaderParamFound := SameText(RttiParameter.Name, EndPointInfo.ResponseHeaderParamInfo.Value.ParamName);
          if ResponseHeaderParamFound then
            Break;
        end;
        if not ResponseHeaderParamFound then
          FMisconfigurations.Add(TClientVirtualApiMisconfiguration.Create(RttiType.QualifiedName, RttiMethod.Name, EndPointInfo.ResponseHeaderParamInfo.Value.ParamName));
      end;

      if EndPointInfo.ConvertResponseForErrorCodeInfo.HasValue then
      begin
        ConvertResponseForErrorCodeFound := False;
        for RttiParameter in RttiMethod.GetParameters do
        begin
          ConvertResponseForErrorCodeFound := SameText(RttiParameter.Name, EndPointInfo.ConvertResponseForErrorCodeInfo.Value.ParamName);
          if ConvertResponseForErrorCodeFound then
            Break;
        end;
        if not ConvertResponseForErrorCodeFound then
          FMisconfigurations.Add(TClientVirtualApiMisconfiguration.Create(RttiType.QualifiedName, RttiMethod.Name, EndPointInfo.ConvertResponseForErrorCodeInfo.Value.ParamName));
      end;
    end;
  end;
end;

constructor TAbstractClientVirtualApi<T, IConfiguration>.Create(const Configuration: IConfiguration);
begin
  inherited Create(DoInvoke);
  Guard.CheckNotNull(Configuration, 'Configuration');
  FConfiguration := Configuration;
end;

procedure TAbstractClientVirtualApi<T, IConfiguration>.ParamNamesToParamNameValues(
   const Arguments: IDictionary<string, TPair<string, TValue>>;
   const Params: IDictionary<string, string>;
   const Kind: TClientVirtualApiCallParameterKind;
   const Call: TClientVirtualApiCall);
var
  MethodParam: string;
  ApiParam: string;
  ParamValue: TPair<string, TValue>;
begin
  for MethodParam in Params.Keys do
    if Arguments.TryGetValue(MethodParam, ParamValue) then
    begin
      Call.SetParameter(Kind, MethodParam, ConvertTValueToString(ParamValue.Value));

      if Params.TryGetValue(MethodParam, ApiParam) then
        Call.SetParameter(Kind, ApiParam, ConvertTValueToString(ParamValue.Value));
    end;
end;

function TAbstractClientVirtualApi<T, IConfiguration>.ProcessPath(
  const Call: TClientVirtualApiCall;
  const EndPoint: string;
  const Arguments: IDictionary<string, TPair<string, TValue>>): string;
var
  ParamName: string;
  ParamValue: TPair<string, TValue>;
  ParamValueString: String;
begin
  Result := EndPoint;
  with TRegEx.Create(MUSTACHE_REGEXPR, [roIgnoreCase, roMultiline]).Matches(Result).GetEnumerator do
  try
    while MoveNext do
    begin
      ParamName := StringReplace(StringReplace(Current.Value, '{', '', [rfReplaceAll]), '}', '', [rfReplaceAll]);
      if Arguments.TryGetValue(ParamName, ParamValue) then
      begin
        ParamValueString := ConvertTValueToString(ParamValue.Value);
        Call.SetParameter(pkPath, ParamName, ParamValueString);
        Result := StringReplace(Result, '{' + ParamName + '}', ParamValueString, [rfReplaceAll]);
      end;
    end;
  finally
    Free;
  end;

  //Format is Json by default, but it could be injected as a parameter or as a Configuration setting
  Result := StringReplace(Result, FORMAT_PARAM, DEFAULT_FORMAT, [rfReplaceAll]);
end;

procedure TAbstractClientVirtualApi<T, IConfiguration>.MapArgumentsAndParameters(
  const Parameters: TArray<TRttiParameter>;
  const Args: TArray<TValue>;
  const Arguments: IDictionary<string, TPair<string, TValue>>);
var
  Index: Integer;
  RttiParameter: TRttiParameter;
  RttiAttribute: TCustomAttribute;
begin
  for Index := 0 to Length(Parameters) - 1 do
  begin
    RttiParameter := Parameters[Index];
    for RttiAttribute in RttiParameter.GetAttributes do
      if RttiAttribute is ApiParamAttribute then
      begin
        Arguments.AddOrSetValue(ApiParamAttribute(RttiAttribute).ParamName, TPair<string, TValue>.Create(string(Args[Index + 1].TypeInfo.Name), Args[Index + 1]));
        Break;
      end;

    Arguments.AddOrSetValue(RttiParameter.Name, TPair<string, TValue>.Create(string(Args[Index + 1].TypeInfo.Name), Args[Index + 1]));
  end;
end;

procedure TAbstractClientVirtualApi<T, IConfiguration>.MapArgumentAndConfiguration(const Arguments: IDictionary<string, TPair<string, TValue>>);
var
  Context: Shared<TRttiContext>;
  ConfigurationRttiType: TRttiType;
  MethodInstanceValue: TValue;
  RttiMethod: TRttiMethod;
  ResultValue: TValue;
  RttiAttribute: TCustomAttribute;
  MethodName: string;
begin
  // Map configuration settings to arguments
  Context := TRttiContext.Create;
  MethodInstanceValue := TValue.From<IConfiguration>(FConfiguration);
  ConfigurationRttiType := Context.Value.GetType(TypeInfo(IConfiguration));
  if Assigned(ConfigurationRttiType) then
    for RttiMethod in ConfigurationRttiType.GetMethods do
      if (RttiMethod.MethodKind = mkFunction) and
         (Length(RttiMethod.GetParameters) = 0) then
      begin
        MethodName := RttiMethod.Name;
        if MethodName.ToUpper.StartsWith('GET') then
          MethodName := Copy(MethodName, 4, Length(MethodName));
        ResultValue := RttiMethod.Invoke(MethodInstanceValue, []);
        Arguments.AddOrSetValue(MethodName, TPair<string, TValue>.Create(string(ResultValue.TypeInfo.Name), ResultValue));

        for RttiAttribute in RttiMethod.GetAttributes do
          if RttiAttribute is ApiParamAttribute then
          begin
            Arguments.AddOrSetValue(ApiParamAttribute(RttiAttribute).ParamName, TPair<string, TValue>.Create(string(ResultValue.TypeInfo.Name), ResultValue));
            Break
          end;
      end;
end;

procedure TAbstractClientVirtualApi<T, IConfiguration>.DoInvoke(
  Method: TRttiMethod;
  const Args: TArray<TValue>;
  out Result: TValue);
var
  EndPointInfo: TClientVirtualApiEndPointInfo;
  Arguments: IDictionary<string, TPair<string, TValue>>;
  ArgumentValue: TPair<string, TValue>;
  Path: string;
  Index: Integer;
  Param: TRttiParameter;
  Call: Shared<TClientVirtualApiCall>;
  ActiveConfig: IActiveClientVirtualApiConfiguration;
begin
  if SameText(Method.Name, 'IsActive') then
  begin
    Result := TValue.From<Boolean>(IsActive);
    Exit;
  end
  else if SameText(Method.Name, 'GetLastStatusCode') then
  begin
    Result := TValue.From<Integer>(FLastStatusCode);
    Exit;
  end;

  Supports(FConfiguration, IActiveClientVirtualApiConfiguration, ActiveConfig);

  if not FEndPointInfo.TryGetValue(Method.Name, EndPointInfo) then
    EndPointInfo := InspectMethod(Method);

  Call := TClientVirtualApiCall.Create(FShortApiName, Method.Name, EndPointInfo.Method);

  Path := '';
  Arguments := TCollections.CreateDictionary<string, TPair<string, TValue>>(TIStringComparer.Ordinal);

  // content type
  Call.Value.ContentType := CONTENT;
  if not EndPointInfo.Content.IsEmpty then
    Call.Value.ContentType := EndPointInfo.Content;

  // Map arguments and parameters
  MapArgumentsAndParameters(Method.GetParameters, Args, Arguments);

  // Map configuration settings to arguments
  MapArgumentAndConfiguration(Arguments);

  // Path
  Path := ProcessPath(Call, EndPointInfo.EndPoint, Arguments);

  // Post body
  if (not EndPointinfo.RequestParam.IsEmpty) and
     Arguments.TryGetValue(EndPointinfo.RequestParam, ArgumentValue) then
    Call.Value.PostBody := ConvertRequestDtoToString(ArgumentValue.Value);

  // parameters
  ParamNamesToParamNameValues(Arguments, EndPointInfo.QueryParams, pkQuery, Call);
  ParamNamesToParamNameValues(Arguments, EndPointInfo.HeaderParams, pkHeader, Call);
  ParamNamesToParamNameValues(Arguments, EndPointInfo.FormParams, pkForm, Call);

  // Api of data
  Call.Value.Url := FConfiguration.BaseUrl + Path;
  Call.Value.HandleRedirects := EndPointInfo.HandleRedirects;

  if Assigned(ActiveConfig) then
    ActiveConfig.CallBegins(Call);
  Call.Value.Start;

  CallApi(Call);

  FLastStatusCode := Call.Value.ResponseCode;


  if not Call.Value.IsOk then
  begin
    if EndPointInfo.ConvertResponseForErrorCodeInfo.HasValue and (EndPointInfo.ConvertResponseForErrorCodeInfo.Value.ErrorCode = Call.Value.ResponseCode) then
      for Index := 0 to High(Method.GetParameters) do
      begin
        Param := Method.GetParameters[Index];
        if SameText(Param.Name, EndPointInfo.ConvertResponseForErrorCodeInfo.Value.ParamName) then
        begin
          try
            Args[Index + 1] := ConvertResponseToDto(
              Call.Value.ResponseContent, Param.ParamType.Handle);
          except
            ; // silence conversion errors
          end;
          Break;
        end;
      end;

    raise EFidoClientApiException.Create(Call.Value.ResponseCode, Call.Value.ResponseContent);
  end;

  if EndPointInfo.ResponseHeaderParamInfo.HasValue and (Call.Value.ResponseCode = EndPointInfo.ResponseHeaderParamInfo.Value.ResponseCode) then
    for Index := 0 to High(Method.GetParameters) do
      if SameText(Method.GetParameters[Index].Name, EndPointInfo.ResponseHeaderParamInfo.Value.FParamName) then
      begin
        Args[Index + 1] := Call.Value.ResponseHeaders.Values[EndPointInfo.ResponseHeaderParamInfo.Value.FHeaderParam];
        Break;
      end;

  // Build the function result, if necessary
  if (Method.MethodKind = mkFunction) and
     (Method.ReturnType.TypeKind = tkInterface) then
    Result := ConvertResponseToDto(Call.Value.ResponseContent, Method.ReturnType.Handle);

  if Assigned(ActiveConfig) then
    ActiveConfig.CallEnded(Call.Value)
end;

class procedure TAbstractClientVirtualApi<T, IConfiguration>.InspectInterface(const ApiInterface: TRttiType);
var
  Attribute : TCustomAttribute;
begin
  for Attribute in ApiInterface.GetAttributes do
    if Attribute is ApiVersionAttribute then
    begin
      FApiVersion := ApiVersionAttribute(Attribute).Version;
      Break;
    end;
end;

class function TAbstractClientVirtualApi<T, IConfiguration>.InspectMethod(const Method: TRttiMethod): TClientVirtualApiEndPointInfo;
var
  Attribute : TCustomAttribute;
  EndPointInfo: TClientVirtualApiEndPointInfo;

  RequestMethod: TRESTRequestMethod;
  EndPoint: string;
  QueryParams: IDictionary<string, string>;
  HeaderParams: IDictionary<string, string>;
  FormParams: IDictionary<string, string>;
  FileParams: IDictionary<string, string>;
  RequestParam: string;
  Content: string;
  HandleRedirects: boolean;
  ResponseHeaderParamInfo: Nullable<TResponseHeaderParamInfo>;
  ConvertResponseForErrorCodeInfo: Nullable<TConvertResponseForErrorCodeInfo>;
begin
  if FEndPointInfo.TryGetValue(Method.Name, EndPointInfo) then
    Exit(EndPointInfo);

  RequestMethod := rmPATCH;
  RequestParam := '';
  Content := '';
  HandleRedirects := true;

  QueryParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);
  HeaderParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);
  FormParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);
  FileParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);

  for Attribute in Method.GetAttributes do
    if Attribute.InheritsFrom(ClientVirtualApiAttribute) then
      if Attribute is EndpointAttribute then
      begin
        RequestMethod := EndpointAttribute(Attribute).Method;
        EndPoint := EndpointAttribute(Attribute).EndPoint;
      end
      else if Attribute is QueryParamAttribute then
        QueryParams.AddOrSetValue(QueryParamAttribute(Attribute).MethodParam, QueryParamAttribute(Attribute).ApiParam)
      else if Attribute is HeaderParamAttribute then
        HeaderParams.AddOrSetValue(HeaderParamAttribute(Attribute).MethodParam, HeaderParamAttribute(Attribute).ApiParam)
      else if Attribute is FormParamAttribute then
        FormParams.AddOrSetValue(FormParamAttribute(Attribute).MethodParam, FormParamAttribute(Attribute).ApiParam)
      else if Attribute is FileParamAttribute then
        FileParams.AddOrSetValue(FileParamAttribute(Attribute).MethodParam, FileParamAttribute(Attribute).ApiParam)
      else if Attribute is RequestParamAttribute then
        RequestParam := RequestParamAttribute(Attribute).MethodParam
      else if Attribute is ContentAttribute then
        Content := ContentAttribute(Attribute).Content
      else if Attribute is ResponseHeaderParamAttribute then
        ResponseHeaderParamInfo := TResponseHeaderParamInfo.Create(
          ResponseHeaderParamAttribute(Attribute).ResponseCode,
          ResponseHeaderParamAttribute(Attribute).HeaderParam,
          ResponseHeaderParamAttribute(Attribute).ParamName)
      else if Attribute is ConvertResponseForErrorCodeAttribute then
        ConvertResponseForErrorCodeInfo :=
          TConvertResponseForErrorCodeInfo.Create(ConvertResponseForErrorCodeAttribute(Attribute).ErrorCode, ConvertResponseForErrorCodeAttribute(Attribute).ParamName)
      else if Attribute is DisableRedirectsAttribute then
        HandleRedirects := false;

  Result := TClientVirtualApiEndPointInfo.Create(
    Method.Parent.QualifiedName,
    RequestMethod,
    EndPoint,
    QueryParams,
    HeaderParams,
    FormParams,
    FileParams,
    RequestParam,
    Content,
    ResponseHeaderParamInfo,
    ConvertResponseForErrorCodeInfo,
    HandleRedirects);
  FEndPointInfo.AddOrSetValue(Method.Name, Result);
end;

function TAbstractClientVirtualApi<T, IConfiguration>.IsActive: Boolean;
begin
  Result := FConfiguration.Active;
end;

{ TVirtualApiEndPointInfo }

constructor TClientVirtualApiEndPointInfo.Create(
  const ApiName: string;
  const Method: TRESTRequestMethod;
  const EndPoint: string;
  const QueryParams: IDictionary<string, string>;
  const HeaderParams: IDictionary<string, string>;
  const FormParams: IDictionary<string, string>;
  const FileParams: IDictionary<string, string>;
  const RequestParam: string;
  const Content: string;
  const ResponseHeaderParamInfo: Nullable<TResponseHeaderParamInfo>;
  const ConvertResponseForErrorCodeInfo: Nullable<TConvertResponseForErrorCodeInfo>;
  const HandleRedirects: boolean);
begin
  FApiName := ApiName;
  FMethod := Method;
  FEndPoint :=  EndPoint;
  FQueryParams := QueryParams;
  FHeaderParams := HeaderParams;
  FFormParams := FormParams;
  FFileParams := FileParams;
  FRequestParam := RequestParam;
  FContent := Content;
  FResponseHeaderParamInfo := ResponseHeaderParamInfo;
  FConvertResponseForErrorCodeInfo := ConvertResponseForErrorCodeInfo;
  FHandleRedirects := HandleRedirects;
end;

{ TVirtualApiMisconfiguration }

constructor TClientVirtualApiMisconfiguration.Create(
  const ApiName: string;
  const ApiMethod: string;
  const ParameterName: string);
begin
  FApiName := ApiName;
  FApiMethod := ApiMethod;
  FParameterName := ParameterName;
end;

{ TResponseHeaderParamInfo }

constructor TResponseHeaderParamInfo.Create(
  const ResponseCode: Integer;
  const HeaderParam: string;
  const ParamName: string);
begin
  FResponseCode := ResponseCode;
  FHeaderParam := HeaderParam;
  FParamName := ParamName;
end;

{ TConvertResponseForErrorCodeInfo }

constructor TConvertResponseForErrorCodeInfo.Create(
  const ErrorCode: Integer;
  const ParamName: string);
begin
  FErrorCode := ErrorCode;
  FParamName := ParamName;
end;

end.
