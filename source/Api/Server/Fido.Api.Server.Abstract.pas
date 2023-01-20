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

unit Fido.Api.Server.Abstract;

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.RegularExpressions,
  System.Rtti,
  System.TypInfo,

  Generics.Defaults,
  Generics.Collections,

  Spring.Collections,
  Spring.Logging,

  Fido.Exceptions,
  Fido.Utilities,
  Fido.JSON.Marshalling,

  Fido.Api.Server.Intf,
  Fido.Api.Server.Exceptions,
  Fido.Api.Server.Resource.Attributes,
  Fido.Http.Request.Intf,
  Fido.Http.Response.Intf,
  Fido.Http.Types,
  Fido.Http.Utils,
  Fido.Web.Server.Intf,
  Fido.Web.Server.Null;

type
  EFidoApiException = class(EFidoException);

  TAbstractApiServer  = class abstract(TInterfacedObject, IApiServer)
  private var
    FWebServer: IWebServer;
    FResources: IList<TObject>;
    FEndPoints: IDictionary<string, IDictionary<THttpMethod, TEndPoint>>;
    FRequestMiddlewares: IDictionary<string, TApiRequestMiddlewareFunc>;
    FResponseMiddlewares: IDictionary<string, TApiResponseMiddlewareProc>;
    FExceptionMiddlewareProc: TApiExceptionMiddlewareProc;
    FFormatExceptionToResponseProc: TApiFormatExceptionToResponseProc;
    FGlobalMiddleware: TApiGlobalMiddlewareProc;
    FLock: TLightweightMREW;
    FPort: word;
  private
    function TryGetEndPoint(const ApiRequest: IHttpRequest; out EndPoint: TEndPoint): Boolean;
    function TrySetPathParams(const EndPoint: TEndPoint; const URI: string; const PathParams: IDictionary<string, string>): Boolean;
    function TrySetMethodParams(const ApiRequest: IHttpRequest; const PathParams: IDictionary<string, string>; const EndPoint: TEndPoint; var Params: TArray<TValue>): Boolean;
    procedure UpdateResponse(const Method: TRttiMethod; const MethodResult: TValue; const ApiResponse: IHttpResponse; const EndPoint: TEndPoint; const Params: array of TValue);
  protected
    procedure ProcessCommand(const ApiRequest: IHttpRequest; const ApiResponse: IHttpResponse);

    function JSONConvertRequestToDto(const Request: string; const TypeInfo: PTypeInfo): TValue; virtual;
    procedure ConvertFromStringToTValue(const StringValue: string; const TypeInfo: PTypeInfo; out Value: TValue); virtual;
    function ConvertTValueToString(const Value: TValue): string; virtual;
    function ConvertRequestToDto(const MimeType: TMimeType; const Request: string; const TypeInfo: PTypeInfo): TValue; virtual;
    function ConvertResponseDtoToString(const MimeType: TMimeType; const Value: TValue): string; virtual;
    procedure DoFormatExceptionToResponse(const E: Exception; const ApiResponse: IHttpResponse); virtual;
  public
    constructor Create(const Port: Word; const SSLCertData: TSSLCertData);
    destructor Destroy; override;

    function Port: Word;
    function IsActive: Boolean; virtual; abstract;
    procedure SetActive(const Value: Boolean); virtual; abstract;
    procedure SetWebServer(const WebServer: IWebServer);
    procedure RegisterResource(const Resource: TObject);
    procedure RegisterRequestMiddleware(const Name: string; const Step: TApiRequestMiddlewareFunc);
    procedure RegisterResponseMiddleware(const Name: string; const Step: TApiResponseMiddlewareProc);
    procedure RegisterExceptionMiddleware(const MiddlewareProc: TApiExceptionMiddlewareProc);
    procedure RegisterGlobalMiddleware(const MiddlewareProc: TApiGlobalMiddlewareProc);
    procedure RegisterFormatExceptionToResponse(const FormatExceptionToResponseProc: TApiFormatExceptionToResponseProc);
  end;

implementation

{ TAbstractApiServer }

function TAbstractApiServer.ConvertResponseDtoToString(
  const MimeType: TMimeType;
  const Value: TValue): string;
begin
  case MimeType of
    mtJson: Result := JSONMarshaller.From(Value, Value.TypeInfo);
  end;
end;

function TAbstractApiServer.JSONConvertRequestToDto(
  const Request: string;
  const TypeInfo: PTypeInfo): TValue;
begin
  if not Request.IsEmpty then
    Exit(JSONUnmarshaller.&To(Request, TypeInfo));

  TValue.Make(nil, TypeInfo, Result);
end;

function TAbstractApiServer.ConvertRequestToDto(
  const MimeType: TMimeType;
  const Request: string;
  const TypeInfo: PTypeInfo): TValue;
begin
  case MimeType of
    mtJson, mtAll, mtDefault: Result := JSONConvertRequestToDto(Request, TypeInfo);
  end;
end;

function TAbstractApiServer.ConvertTValueToString(const Value: TValue): string;
begin
  Result := JSONMarshaller.From(Value, Value.TypeInfo);
end;

constructor TAbstractApiServer.Create(
  const Port: Word;
  const SSLCertData: TSSLCertData);
begin
  inherited Create;

  FPort := Port;
  FWebServer := TNullWebServer.Create;

  FEndPoints := TCollections.CreateDictionary<string, IDictionary<THttpMethod, TEndPoint>>(TIStringComparer.Ordinal);
  FResources := TCollections.CreateObjectList<TObject>;

  FRequestMiddlewares := TCollections.CreateDictionary<string, TApiRequestMiddlewareFunc>(TIStringComparer.Ordinal);
  FResponseMiddlewares := TCollections.CreateDictionary<string, TApiResponseMiddlewareProc>(TIStringComparer.Ordinal);
  FExceptionMiddlewareProc := DefaultExceptionMiddlewareProc;
  FGlobalMiddleware := DefaultGlobalMiddlewareProc;
  FFormatExceptionToResponseProc := DefaultFormatExceptionToResponseProc;
end;

destructor TAbstractApiServer.Destroy;
begin
  FEndPoints.Clear;
  FResources.Clear;

  inherited;
end;

procedure TAbstractApiServer.DoFormatExceptionToResponse(const E: Exception; const ApiResponse: IHttpResponse);
begin
  FFormatExceptionToResponseProc(E, ApiResponse);
end;

function TAbstractApiServer.TryGetEndPoint(
  const ApiRequest: IHttpRequest;
  out EndPoint: TEndPoint): Boolean;
var
  Uri: string;
begin
  Result := FEndPoints.Keys.TryGetFirst(
      Uri,
      function(const Item: string): Boolean
      begin
        Result := TRegEx.IsMatch(ApiRequest.URI.ToUpper, Item.ToUpper)
      end) and
    FEndPoints.Items[Uri].TryGetValue(ApiRequest.Method, EndPoint);
end;

function TAbstractApiServer.TrySetPathParams(
  const EndPoint: TEndPoint;
  const URI: string;
  const PathParams: IDictionary<string, string>): Boolean;
var
  LResult: Boolean;
  LEndPoint: TEndPoint;
begin
  LResult := true;
  LEndPoint := EndPoint;
  EndPoint.Parameters
    .Where(
      function(const EndPointParameter: TEndPointParameter): Boolean
      begin
        Result := EndPointParameter.&Type = mptPath;
      end)
    .ForEach(
      procedure(const EndPointParameter: TEndPointParameter)
      var
        RegExpPath: string;
        Path: string;
        ParameterIndex: Integer;
        ParameterValue: string;
        TextBeforeTheParams: string;
        SanitizedEndpointPath: string;
        SanitizedURI: string;
      begin
        TextBeforeTheParams := Copy(LEndPoint.Path, 1, Pos('{', LEndPoint.Path) - 2);
        SanitizedEndpointPath := StringReplace(LEndPoint.Path, TextBeforeTheParams, '', []);
        SanitizedURI := Copy(URI, Pos(TextBeforeTheParams, URI) + Length(TextBeforeTheParams));
        RegExpPath := StringReplace(SanitizedEndpointPath, '/', '\/', [rfReplaceAll]);

        Path := SanitizedEndpointPath;
        ParameterIndex := -1;
        with TRegEx.Matches(RegExpPath, '{[\s\S][^{]+}').GetEnumerator do
        begin
          while MoveNext do
            if ('{' + EndPointParameter.Name.ToUpper + '}') = GetCurrent.Value.ToUpper then
            begin
              ParameterIndex := GetCurrent.Index;
              with PathParams.GetEnumerator do
                while MoveNext do
                begin
                  Path := StringReplace(Path, '{' + Current.Key + '}', Current.Value, [rfIgnoreCase]);
                end;
              ParameterValue := StringReplace(SanitizedURI, Copy(Path, 1, Pos('{' + EndPointParameter.Name.ToUpper + '}', Path.ToUpper) - 1), '', [rfIgnoreCase]);
              if Pos('/', ParameterValue) > 0 then
                ParameterValue := Copy(ParameterValue, 1, Pos('/', ParameterValue) - 1);
            end;
          Free;
        end;

        if ParameterIndex = -1 then
          Exit;

        LResult := True;
        PathParams[EndPointParameter.Name] := ParameterValue;
      end);

  Result := LResult;
end;

procedure TAbstractApiServer.ConvertFromStringToTValue(
  const StringValue: string;
  const TypeInfo: PTypeInfo;
  out Value: TValue);
begin
  Value := JSONUnmarshaller.&To(StringValue, TypeInfo);
end;

function TAbstractApiServer.TrySetMethodParams(
  const ApiRequest: IHttpRequest;
  const PathParams: IDictionary<string, string>;
  const EndPoint: TEndPoint;
  var Params: TArray<TValue>): Boolean;
var
  ParameterIndex: Integer;
  LParams: TArray<TValue>;
  LResult: Boolean;
begin
  LResult := True;
  SetLength(LParams, EndPoint.Parameters.Count);

  ParameterIndex := 0;
  EndPoint.Parameters.ForEach(
    procedure(const Item: TEndPointParameter)
    var
      Value: TValue;
      ParameterValue: string;
      ItemName: string;
    begin
      if not Item.IsOut then
      begin
        ItemName := Item.Name;
        if not Item.RestName.IsEmpty then
          ItemName := Item.RestName;
        case Item.&Type of
          mptPath: begin
            if not PathParams.TryGetValue(ItemName, ParameterValue) then
            begin
              if not Item.IsNullable then
              begin
                LResult := False;
                Exit;
              end;

              ConvertFromStringToTValue('', Item.TypeInfo, Value);
              LParams[ParameterIndex] := Value;
            end
            else
            begin
              ConvertFromStringToTValue(ParameterValue, Item.TypeInfo, Value);
              LParams[ParameterIndex] := Value;
            end;
          end;
          mptForm: begin
            if not ApiRequest.FormParams.TryGetValue(ItemName, ParameterValue) then
            begin
              if not Item.IsNullable then
              begin
                LResult := False;
                Exit;
              end;

              ConvertFromStringToTValue('', Item.TypeInfo, Value);
              LParams[ParameterIndex] := Value;
            end
            else
              ConvertFromStringToTValue(ParameterValue, Item.TypeInfo, Value);
            LParams[ParameterIndex] := Value;
          end;
          mptHeader: begin
            if not ApiRequest.HeaderParams.TryGetValue(ItemName, ParameterValue) then
            begin
              if not Item.IsNullable then
              begin
                LResult := False;
                Exit;
              end;

              ConvertFromStringToTValue('', Item.TypeInfo, Value);
              LParams[ParameterIndex] := Value;
            end
            else
              ConvertFromStringToTValue(ParameterValue, Item.TypeInfo, Value);
            LParams[ParameterIndex] := Value;
          end;
          mptQuery: begin
            if not ApiRequest.QueryParams.TryGetValue(ItemName, ParameterValue) then
            begin
              if not Item.IsNullable then
              begin
                LResult := False;
                Exit;
              end;


              ConvertFromStringToTValue('', Item.TypeInfo, Value);
              LParams[ParameterIndex] := Value;
            end
            else
              ConvertFromStringToTValue(ParameterValue, Item.TypeInfo, Value);
            LParams[ParameterIndex] := Value;
          end;
          mptBody: begin
            if Assigned(Item.ClassType) or Item.IsInterface then
              LParams[ParameterIndex] := ConvertRequestToDto(ApiRequest.MimeType, ApiRequest.Body, Item.TypeInfo);
          end;
        end
      end
      else
      begin
        if Assigned(Item.ClassType) or Item.IsInterface then
          LParams[ParameterIndex] := ConvertRequestToDto(ApiRequest.MimeType, '', Item.TypeInfo)
        else
          LParams[ParameterIndex] := ConvertTValueToString('');
      end;
      Inc(ParameterIndex);
    end);

    Result := LResult;
    Params := LParams;
end;

procedure TAbstractApiServer.UpdateResponse(
  const Method: TRttiMethod;
  const MethodResult: TValue;
  const ApiResponse: IHttpResponse;
  const EndPoint: TEndPoint;
  const Params: array of TValue);
var
  LParams: IList<TValue>;
  LEndPoint: TEndpoint;
begin
  LParams := TCollections.CreateList<TValue>(Params);

  LEndPoint := Endpoint;
  EndPoint.Parameters
    .Where(function(const Item: TEndPointParameter): Boolean
      begin
        Result := Item.IsOut;
      end)
    .ForEach(procedure(const Item: TEndPointParameter)
      var
        ItemName: string;
        ParameterIndex: Integer;
      begin
        ItemName := Item.Name;
        if not Item.RestName.IsEmpty then
          ItemName := Item.RestName;

        ParameterIndex := LEndPoint.Parameters.IndexOf(Item);

        case Item.&Type of
          mptBody:
            if not (Assigned(Item.ClassType) or Item.IsInterface) then
              ApiResponse.SetBody(ConvertTValueToString(LParams[ParameterIndex]))
            else
              ApiResponse.SetBody(ConvertResponseDtoToString(ApiResponse.MimeType, LParams[ParameterIndex]));
          mptHeader: ApiResponse.HeaderParams[ItemName] := ConvertTValueToString(LParams[ParameterIndex]).DeQuotedString('"');
        end;
      end);

  if Method.MethodKind = mkFunction then
    if MethodResult.IsObject or MethodResult.IsArray then
      ApiResponse.SetBody(ConvertResponseDtoToString(ApiResponse.MimeType, MethodResult))
    else
      ApiResponse.SetBody(ConvertTValueToString(MethodResult));
  ApiResponse.SetResponseCode(EndPoint.ResponseCode, EndPoint.ResponseText);
end;

function TAbstractApiServer.Port: Word;
begin
  Result := FPort;
end;

procedure TAbstractApiServer.ProcessCommand(
  const ApiRequest: IHttpRequest;
  const ApiResponse: IHttpResponse);
var
  EndPoint: TEndPoint;
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  PathParams: IDictionary<string, string>;
begin
  PathParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);

  try
    if ApiRequest.Method = rmUnknown then
      raise EApiServer404.Create('Invalid HTTP method.');

    if ApiRequest.MimeType = mtUnknown then
      raise EApiServer400.Create('Request mime type not supported.');

    if ApiResponse.MimeType = mtUnknown then
     raise EApiServer400.Create('Response mime type not supported.');

    if FWebServer.Process(ApiRequest, ApiResponse) then
    begin
      ApiResponse.SetResponseCode(200, 'OK');
      Exit;
    end;

    if not TryGetEndPoint(ApiRequest, EndPoint) then
      raise EApiServer404.Create('Endpoint not found.');

    if (ApiResponse.MimeType = mtAll) and (High(EndPoint.Produces)>=0) then
      ApiResponse.SetMimeType(EndPoint.Produces[0]);
    RttiType := RttiContext.GetType(EndPoint.Instance.AsObject.ClassType);

    TCollections.CreateList<TRttiMethod>(RttiType.GetMethods)
      .Where(function(const Method: TRttiMethod): Boolean
        begin
          Result := UpperCase(Method.Name) = UpperCase(EndPoint.MethodName);
        end)
      .ForEach(procedure(const Method: TRttiMethod)
        var
          Params: TArray<TValue>;
          Param: TValue;
          Step: TPair<string, string>;
          PApiepFunc: TApiRequestMiddlewareFunc;
          PostStepProc: TApiResponseMiddlewareProc;
          ResponseCode: Integer;
          ResponseText: string;
          Result: TValue;
        begin
          if not TrySetPathParams(EndPoint, ApiRequest.URI, PathParams) then
            raise EApiServer400.Create('Bad number or type of path parameters.');

          if not TrySetMethodParams(ApiRequest, PathParams, EndPoint, Params) then
            raise EApiServer400.Create('Bad number or type of parameters.');

          try
            try
              for Step in Endpoint.PreProcessPipelineSteps do
                if not FRequestMiddlewares.TryGetValue(Step.Key, PApiepFunc) then
                  raise EFidoApiException.Create('RequestMiddleware ' + Step.Key + ' not found.')
                else if not PApiepFunc(Step.Value, ApiRequest, ResponseCode, ResponseText) then
                begin
                  ApiResponse.SetResponseCode(ResponseCode, ResponseText);
                  Exit;
                end;

              if Endpoint.ExcludeGlobalMiddleware then
                Result := Method.Invoke(EndPoint.Instance, Params)
              else
                FGlobalMiddleware(procedure
                  begin
                    Result := Method.Invoke(EndPoint.Instance, Params);
                  end,
                  EndPoint.Instance.AsObject.ClassName,
                  EndPoint.MethodName);

              for Step in Endpoint.PostProcessPipelineSteps do
              begin
                if not FResponseMiddlewares.TryGetValue(Step.Key, PostStepProc) then
                  raise EFidoApiException.Create('ResponseMiddleware ' + Step.Key + ' not found.');
                PostStepProc(Step.Value, ApiRequest, ApiResponse)
              end;

              UpdateResponse(Method, Result, ApiResponse, EndPoint, Params);
            except
              on E: Exception do
            begin
                FExceptionMiddlewareProc(E);
              raise;
            end;
            end;
          finally
            if Result.IsObject then
              Result.AsObject.Free;
            for Param in Params do
              if Param.IsObject then
                Param.AsObject.Free;
          end;
      end);
  except
    on E: EApiServer do
    begin
      ApiResponse.SetResponseCode(E.Code, E.ShortMsg);
      raise;
    end
    else
      raise;
  end;
end;

procedure TAbstractApiServer.RegisterResource(const Resource: TObject);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  ResourcePath: string;
  MethodPath: string;
  ApiMethod: THttpMethod;
  RegExpPath: string;
  SubDictionary: IDictionary<THttpMethod, TEndPoint>;
  Consumes: TArray<TMimeType>;
  Produces: TArray<TMimeType>;
  ResponseCode: Integer;
  ResponseText: string;
begin
  FResources.Add(Resource);

  Consumes := [];
  Produces := [];

  RttiType := Context.GetType(Resource.ClassType);

  TCollections.CreateList<TCustomAttribute>(RttiType.GetAttributes).ForEach(
    procedure(const Attribute: TCustomAttribute)
    begin
      if Attribute is BaseUrlAttribute then
        ResourcePath := (Attribute as BaseUrlAttribute).BaseUrl
      else if Attribute is ConsumesAttribute then
      begin
        SetLength(Consumes, Length(Consumes) + 1);
        Consumes[High(Consumes)] := (Attribute as ConsumesAttribute).MimeType
      end
      else if Attribute is ProducesAttribute then
      begin
        SetLength(Produces, Length(Produces) + 1);
        Produces[High(Produces)] := (Attribute as ProducesAttribute).MimeType
      end;
    end);

  if not ResourcePath.StartsWith('/') then
    ResourcePath := '/' + ResourcePath;

  TCollections.CreateList<TRttiMethod>(RttiType.GetMethods).ForEach(
    procedure(const Method: TRttiMethod)
    var
      Parameters: IList<TEndPointParameter>;
      PreProcessPipelineSteps: IList<TPair<string, string>>;
      PostProcessPipelineSteps: IList<TPair<string, string>>;
      FlagExcludeGlobalMiddleware: Boolean;
    begin
      ResponseCode := 200;
      ResponseText := 'OK';

      ApiMethod := rmUnknown;
      MethodPath := '';
      Parameters := TCollections.CreateList<TEndPointParameter>;
      PreProcessPipelineSteps := TCollections.CreateList<TPair<string, string>>;
      PostProcessPipelineSteps := TCollections.CreateList<TPair<string, string>>;
      FlagExcludeGlobalMiddleware := False;

      TCollections.CreateList<TCustomAttribute>(Method.GetAttributes).ForEach(
        procedure(const Attribute: TCustomAttribute)
        begin
          if Attribute is PathAttribute then
          begin
            MethodPath := (Attribute as PathAttribute).Path;
            ApiMethod := (Attribute as PathAttribute).Method;
          end
          else if Attribute is ResponseCodeAttribute then
          begin
            ResponseCode := (Attribute as ResponseCodeAttribute).ResponseCode;
            ResponseText := (Attribute as ResponseCodeAttribute).ResponseText;
          end
          else if Attribute is RequestMiddlewareAttribute then
            PreProcessPipelineSteps.Add(TPair<string, string>.Create((Attribute as RequestMiddlewareAttribute).StepName, (Attribute as RequestMiddlewareAttribute).CommaSeparatedParams))
          else if Attribute is ResponseMiddlewareAttribute then
            PostProcessPipelineSteps.Add(TPair<string, string>.Create((Attribute as ResponseMiddlewareAttribute).StepName, (Attribute as ResponseMiddlewareAttribute).CommaSeparatedParams))
          else if Attribute is ExcludeGlobalMiddlewareAttribute then
            FlagExcludeGlobalMiddleware := True;
        end);

      if (MethodPath.IsEmpty) and
         (ApiMethod = rmUnknown) then
        Exit;

      TCollections.CreateList<TRttiParameter>(Method.GetParameters).ForEach(
        procedure(const Parameter: TRttiParameter)
        var
          ClassType: TClass;
          IsInterface: Boolean;
          IsNullable: Boolean;
          OutParameter: Boolean;
          ParameterType: TMethodParameterType;
          TypeQualifiedName: string;
          ApiParameterName: string;
        begin
          ClassType := nil;
          if Parameter.ParamType.IsInstance then
            ClassType := Parameter.ParamType.AsInstance.MetaclassType;
          IsNullable := Parameter.ParamType.ToString.ToUpper.Contains('NULLABLE<');
          IsInterface := Parameter.ParamType.TypeKind = tkInterface;

          TypeQualifiedName := Parameter.QualifiedClassName;

          OutParameter := (pfVar in Parameter.Flags) or
                          (pfOut in Parameter.Flags);
          ParameterType := mptUnknown;

          TCollections.CreateList<TCustomAttribute>(Parameter.GetAttributes).ForEach(
            Procedure(const Attribute: TCustomAttribute)
            begin
              if Attribute is PathParamAttribute then
              begin
                ParameterType := mptPath;
                 ApiParameterName := (Attribute as ParamAttribute).ParamName;
              end
              else if Attribute is FormParamAttribute then
              begin
                 ParameterType := mptForm;
                ApiParameterName := (Attribute as ParamAttribute).ParamName;
              end
              else if Attribute is BodyParamAttribute then
              begin
                ParameterType := mptBody;
                ApiParameterName := (Attribute as ParamAttribute).ParamName;
              end
              else if Attribute is HeaderParamAttribute then
              begin
                ParameterType := mptHeader;
                ApiParameterName := (Attribute as ParamAttribute).ParamName;
              end
              else if Attribute is QueryParamAttribute then
              begin
                ParameterType := mptQuery;
                ApiParameterName := (Attribute as ParamAttribute).ParamName;
              end;
            end);

          if ApiParameterName.IsEmpty then
            ApiParameterName := Parameter.Name;

          Parameters.Add(
            TEndPointParameter.Create(
              OutParameter,
              Parameter.Name,
              ApiParameterName,
              ClassType,
              IsInterface,
              ParameterType,
              Parameter.ParamType.Handle,
              IsNullable,
              TypeQualifiedName));
        end);

      MethodPath := ResourcePath + MethodPath;
      RegExpPath := TranslatePathToRegEx(MethodPath);

      if (not MethodPath.IsEmpty) and
         (ApiMethod <> rmUnknown) then
      begin
        if not FEndPoints.TryGetValue(RegExpPath, SubDictionary) then
        begin
          SubDictionary := TCollections.CreateDictionary<THttpMethod, TEndPoint>;
          FEndPoints.Add(RegExpPath, SubDictionary);
        end;

        SubDictionary[ApiMethod] :=
          TEndPoint.Create(
            Resource,
            Method.Name,
            MethodPath,
            ApiMethod,
            Parameters,
            Consumes,
            Produces,
            ResponseCode,
            ResponseText,
            PreProcessPipelineSteps,
            PostProcessPipelineSteps,
            FlagExcludeGlobalMiddleware);
      end;
    end);
end;

procedure TAbstractApiServer.RegisterExceptionMiddleware(const MiddlewareProc: TApiExceptionMiddlewareProc);
begin
  FLock.BeginWrite;
  try
    FExceptionMiddlewareProc := MiddlewareProc;
  finally
    FLock.EndWrite;
  end;
end;

procedure TAbstractApiServer.RegisterFormatExceptionToResponse(const FormatExceptionToResponseProc: TApiFormatExceptionToResponseProc);
begin
  FLock.BeginWrite;
  try
    FFormatExceptionToResponseProc := FormatExceptionToResponseProc;
  finally
    FLock.EndWrite;
  end;
end;

procedure TAbstractApiServer.RegisterGlobalMiddleware(const MiddlewareProc: TApiGlobalMiddlewareProc);
begin
  FLock.BeginWrite;
  try
    FGlobalMiddleware := MiddlewareProc;
  finally
    FLock.EndWrite;
  end;
end;

procedure TAbstractApiServer.RegisterRequestMiddleware(
  const Name: string;
  const Step: TApiRequestMiddlewareFunc);
begin
  FRequestMiddlewares.Add(Name, Step);
end;

procedure TAbstractApiServer.RegisterResponseMiddleware(
  const Name: string;
  const Step: TApiResponseMiddlewareProc);
begin
  FResponseMiddlewares.Add(Name, Step);
end;

procedure TAbstractApiServer.SetWebServer(const WebServer: IWebServer);
begin
  if IsActive then
    raise EApiServer.Create('Cannot set the webserver when the Api server is active.');
  FWebServer := Utilities.CheckNotNullAndSet(WebServer, 'Webserver');
end;

end.

