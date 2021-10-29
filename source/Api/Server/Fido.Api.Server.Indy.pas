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

unit Fido.Api.Server.Indy;

interface

uses
  System.Classes,
  System.SysUtils,
  System.RegularExpressions,
  System.Rtti,
  System.TypInfo,
  System.JSON,
  System.Variants,

  Generics.Defaults,
  Generics.Collections,

  idHttpServer,
  idContext,
  IdCustomHTTPServer,
  IdGlobal,
  IdGlobalProtocols,
  IdURI,
  IdSSLOpenSSL,

  Spring,
  Spring.Collections,

  Fido.Exceptions,
  Fido.JSON.Marshalling,

  Fido.Api.Server.Intf,
  Fido.Api.Server.Exceptions,
  Fido.Api.Server.Resource.Attributes,
  Fido.Http.Request.Intf,
  Fido.Http.RequestInfo.Intf,
  Fido.Http.Request,
  Fido.Http.Response.Intf,
  Fido.Http.ResponseInfo.Intf,
  Fido.Http.Response,
  Fido.Http.Types,
  Fido.Web.Server.Websocket.Loop.Abstract,
  Fido.Web.Server.Intf,
  Fido.Web.Server.WebSocket.Loop.Intf,
  Fido.Web.Server.WebSocket.Tool,
  Fido.DesignPatterns.Adapter.TIdHTTPRequestInfoAsIHTTPRequestInfo,
  Fido.DesignPatterns.Adapter.TIdHTTPResponseInfoAsIHTTPResponseInfo;

type
  {$M+}
  TIndyApiServerRequestFactory = reference to function(const RequestInfo: TIdHTTPRequestInfo): IHttpRequest;

  TIndyApiServerResponseFactory = reference to function(const Context: TIdContext; const RequestInfo: TIdHTTPRequestInfo; const ResponseInfo: TIdHTTPResponseInfo): IHttpResponse;

  EFidoApiException = class(EFidoException);

  TIndyApiServer = class(TInterfacedObject, IApiServer)
  private const
    SSL_PORT = 443;
  private
    FPort: word;
    FHttpServer: TIdHTTPServer;
    FSSLIOHandler: TIdServerIOHandlerSSLOpenSSL;
    FWebServer: IWebServer;
    FResources: IList<TObject>;
    FEndPoints: IDictionary<string, IDictionary<THttpMethod, TEndPoint>>;
    FApiRequestFactory: TIndyApiServerRequestFactory;
    FApiResponseFactory: TIndyApiServerResponseFactory;
    FRequestMiddlewares: IDictionary<string, TRequestMiddlewareFunc>;
    FResponseMiddlewares: IDictionary<string, TResponseMiddlewareProc>;
    FLock: IReadWriteSync;
    FWebSockets: IDictionary<string, TClass>;

    function GetDefaultApiRequestFactory: TIndyApiServerRequestFactory;
    function GetDefaultApiResponseFactory: TIndyApiServerResponseFactory;
    procedure OnParseAuthentication(Context: TIdContext; const AuthType: string; const AuthData: string; var Username: string; var Password: string; var Handled: Boolean);
    procedure OnHTTPCommandEvent(Context: TIdContext; RequestInfo: TIdHTTPRequestInfo; ResponseInfo: TIdHTTPResponseInfo);
    function OnVerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth: integer; AError: Integer): Boolean;
    procedure OnQuerySSLPort(APort: Word; var VUseSSL: Boolean);
    procedure ProcessCommand(const ApiRequest: IHttpRequest; const ApiResponse: IHttpResponse);
    function TryGetEndPoint(const ApiRequest: IHttpRequest; out EndPoint: TEndPoint): Boolean;
    function TrySetPathParams(const EndPoint: TEndPoint; const URI: string; const PathParams: IDictionary<string, string>): Boolean;
    function TrySetMethodParams(const ApiRequest: IHttpRequest; const PathParams: IDictionary<string, string>; const EndPoint: TEndPoint; var Params: TArray<TValue>): Boolean;
    procedure UpdateResponse(const Method: TRttiMethod; const MethodResult: TValue; const ApiResponse: IHttpResponse; const EndPoint: TEndPoint; const Params: array of TValue);
    function JSONConvertRequestToDto(const Request: string; const TypeInfo: PTypeInfo): TValue;
    function TranslatePathToRegEx(Path: string): string;
    procedure ConvertFromStringToTValue(const StringValue: string; const TypeInfo: PTypeInfo; out Value: TValue);
  protected
    function ConvertTValueToString(const Value: TValue): string; virtual;
    function ConvertRequestToDto(const MimeType: TMimeType; const Request: string; const TypeInfo: PTypeInfo): TValue; virtual;
    function ConvertResponseDtoToString(const MimeType: TMimeType; const Value: TValue): string; virtual;
  public
    constructor Create(const Port: Word; const MaxConnections: Integer; const WebServer: IWebServer; const SSLCertData: TSSLCertData; const ApiRequestFactory: TIndyApiServerRequestFactory = nil;
      const ApiResponseFactory: TIndyApiServerResponseFactory = nil);
    destructor Destroy; override;

    function IsActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure RegisterResource(const Resource: TObject);
    procedure RegisterWebSocket(const WebSocketClass: TClass);
    procedure RegisterRequestMiddleware(const Name: string; const Step: TRequestMiddlewareFunc);
    procedure RegisterResponseMiddleware(const Name: string; const Step: TResponseMiddlewareProc);
  end;

implementation

{ TIndyApiServer }

function TIndyApiServer.ConvertResponseDtoToString(
  const MimeType: TMimeType;
  const Value: TValue): string;
var
  ParameterIndex: Integer;
  ArrayElement: TValue;
  JsonArray: Shared<TJsonArray>;
begin
  case MimeType of
    mtJson: begin
      if Value.IsObject and (Value.AsType<TObject> <> nil) then
        Result := JSONMarshaller.From(Value, Value.TypeInfo)
      else if Value.IsArray then
      begin
        JsonArray := TJsonArray.Create;

        for ParameterIndex := 0 to Value.GetArrayLength - 1 do
        begin
          ArrayElement := Value.GetArrayElement(ParameterIndex);;
          if ArrayElement.IsObject then
            JsonArray.Value.AddElement(TJSONObject.ParseJSONValue(ConvertResponseDtoToString(MimeType, ArrayElement)) as TJSONValue)
          else
            JsonArray.Value.AddElement(TJSONObject.ParseJSONValue(ConvertTValueToString(ArrayElement)) as TJSONValue);
        end;
        Result := JsonArray.Value.ToString;
      end;
    end;
  end;
end;

function TIndyApiServer.JSONConvertRequestToDto(
  const Request: string;
  const TypeInfo: PTypeInfo): TValue;
begin
  if Request.IsEmpty then
    Exit(nil);

  Result := JSONUnmarshaller.&To(Request, TypeInfo);
end;

function TIndyApiServer.ConvertRequestToDto(
  const MimeType: TMimeType;
  const Request: string;
  const TypeInfo: PTypeInfo): TValue;
begin
  case MimeType of
    mtJson, mtAll: Result := JSONConvertRequestToDto(Request, TypeInfo);
  end;
end;

function TIndyApiServer.ConvertTValueToString(const Value: TValue): string;
begin
  Result := JSONMarshaller.From(Value, Value.TypeInfo);
end;

constructor TIndyApiServer.Create(
  const Port: Word;
  const MaxConnections: Integer;
  const WebServer: IWebServer;
  const SSLCertData: TSSLCertData;
  const ApiRequestFactory: TIndyApiServerRequestFactory;
  const ApiResponseFactory: TIndyApiServerResponseFactory);
var
  UseSSL: Boolean;
begin
  inherited Create;
  FLock := TMREWSync.Create;
  FWebSockets := TCollections.CreateDictionary<string, TClass>(TIStringComparer.Ordinal);

  UseSSL := SSLCertData.IsValid;

  FPort := Port;
  FApiRequestFactory := ApiRequestFactory;
  FApiResponseFactory := ApiResponseFactory;

  FWebServer := WebServer;

  if not Assigned(FApiRequestFactory) then
    FApiRequestFactory := GetDefaultApiRequestFactory();
  if not Assigned(FApiResponseFactory) then
    FApiResponseFactory := GetDefaultApiResponseFactory();

  if UseSSL then
  begin
    FSSLIOHandler := TIdServerIOHandlerSSLOpenSSL.Create(nil);
    FSSLIOHandler.SSLOptions.RootCertFile := SSLCertData.SSLRootCertFilePath;
    FSSLIOHandler.SSLOptions.CertFile := SSLCertData.SSLCertFilePath;
    FSSLIOHandler.SSLOptions.KeyFile := SSLCertData.SSLKeyFilePath;
    FSSLIOHandler.SSLOptions.Method := sslvTLSv1;
    FSSLIOHandler.SSLOptions.Mode := sslmUnassigned;
    FSSLIOHandler.OnGetPassword := nil;
    FSSLIOHandler.OnVerifyPeer := OnVerifyPeer;
  end;

  FHttpServer := TIdHTTPServer.Create(nil);
  FHttpServer.OnCommandGet := OnHTTPCommandEvent;
  FHttpServer.OnCommandOther := OnHTTPCommandEvent;
  FHttpServer.OnParseAuthentication := OnParseAuthentication;
  if UseSSL then
  begin
    FHttpServer.IOhandler := FSSLIOHandler;
    FHttpServer.OnQuerySSLPort := OnQuerySSLPort;
    FHttpServer.DefaultPort := SSL_PORT;
  end
  else
    FHttpServer.Bindings.Add.Port := FPort;
  FHttpServer.MaxConnections := MaxConnections;
  FHttpServer.Active := False;

  FEndPoints := TCollections.CreateDictionary<string, IDictionary<THttpMethod, TEndPoint>>(TIStringComparer.Ordinal);
  FResources := TCollections.CreateObjectList<TObject>;

  FRequestMiddlewares := TCollections.CreateDictionary<string, TRequestMiddlewareFunc>(TIStringComparer.Ordinal);
  FResponseMiddlewares := TCollections.CreateDictionary<string, TResponseMiddlewareProc>(TIStringComparer.Ordinal);
end;

destructor TIndyApiServer.Destroy;
begin
  FEndPoints.Clear;
  FResources.Clear;

  FHttpServer.Active := False;
  FHttpServer.Free;
  FSSLIOHandler.Free;
  inherited;
end;

function TIndyApiServer.GetDefaultApiRequestFactory: TIndyApiServerRequestFactory;
begin
  Result := function(const RequestInfo: TIdHTTPRequestInfo): IHttpRequest
    var
      RequestInfoDecorator: IHttpRequestInfo;
    begin
      RequestInfoDecorator := TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.Create(RequestInfo);
      Result := THttpRequest.Create(RequestInfoDecorator);
    end;
end;

function TIndyApiServer.GetDefaultApiResponseFactory: TIndyApiServerResponseFactory;
begin
  Result := function(
      const Context: TIdContext;
      const RequestInfo: TIdHTTPRequestInfo;
      const ResponseInfo: TIdHTTPResponseInfo): IHttpResponse
    var
      RequestInfoDecorator: IHttpRequestInfo;
      ResponseInfoDecorator: IHttpResponseInfo;
    begin
      RequestInfoDecorator := TIdHTTPRequestInfoAsIHTTPRequestInfoDecorator.Create(RequestInfo);
      ResponseInfoDecorator := TIdHTTPResponseInfoAsIHTTPResponseInfoDecorator.Create(Context, RequestInfo, ResponseInfo);
      Result := THttpResponse.Create(RequestInfoDecorator, ResponseInfoDecorator);
    end;
end;

function TIndyApiServer.IsActive: Boolean;
begin
  Result := FHttpServer.Active;
end;

function TIndyApiServer.TryGetEndPoint(
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

function TIndyApiServer.TrySetPathParams(
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
      begin
        RegExpPath := StringReplace(LEndPoint.Path, '/', '\/', [rfReplaceAll]);

        Path := LEndPoint.Path;
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
              ParameterValue := StringReplace(URI, Copy(Path, 1, Pos('{' + EndPointParameter.Name.ToUpper + '}', Path.ToUpper) - 1), '', [rfIgnoreCase]);
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

procedure TIndyApiServer.ConvertFromStringToTValue(
  const StringValue: string;
  const TypeInfo: PTypeInfo;
  out Value: TValue);
begin
  Value := JSONUnmarshaller.&To(StringValue, TypeInfo);
end;

function TIndyApiServer.TrySetMethodParams(
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
    begin
      if not Item.IsOut then
      begin
        case Item.&Type of
          mptPath: begin
            if not PathParams.TryGetValue(Item.Name, ParameterValue) then
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
            if not ApiRequest.FormParams.TryGetValue(Item.Name, ParameterValue) then
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
            if not ApiRequest.HeaderParams.TryGetValue(Item.Name, ParameterValue) then
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
            if not ApiRequest.QueryParams.TryGetValue(Item.Name, ParameterValue) then
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

procedure TIndyApiServer.UpdateResponse(
  const Method: TRttiMethod;
  const MethodResult: TValue;
  const ApiResponse: IHttpResponse;
  const EndPoint: TEndPoint;
  const Params: array of TValue);
var
  ParameterIndex: Integer;
  LParams: IList<TValue>;
begin
  ParameterIndex := 0;
  LParams := TCollections.CreateList<TValue>(Params);

  EndPoint.Parameters
    .Where(function(const Item: TEndPointParameter): Boolean
      begin
        Result := Item.IsOut;
      end)
    .ForEach(procedure(const Item: TEndPointParameter)
      begin
        case Item.&Type of
          mptBody:
            if not (Assigned(Item.ClassType) or Item.IsInterface) then
              ApiResponse.SetBody(ConvertTValueToString(LParams[ParameterIndex]))
            else
              ApiResponse.SetBody(ConvertResponseDtoToString(ApiResponse.MimeType, LParams[ParameterIndex]));
          mptHeader: ApiResponse.HeaderParams[Item.Name] := ConvertTValueToString(LParams[ParameterIndex]);
        end;
        Inc(ParameterIndex);
      end);

  if Method.MethodKind = mkFunction then
    if MethodResult.IsObject or MethodResult.IsArray then
      ApiResponse.SetBody(ConvertResponseDtoToString(ApiResponse.MimeType, MethodResult))
    else
      ApiResponse.SetBody(ConvertTValueToString(MethodResult));
  ApiResponse.SetResponseCode(EndPoint.ResponseCode, EndPoint.ResponseText);
end;

procedure TIndyApiServer.ProcessCommand(
  const ApiRequest: IHttpRequest;
  const ApiResponse: IHttpResponse);
var
  EndPoint: TEndPoint;
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  PathParams: IDictionary<string, string>;
  WebSocket: ILoopServerWebSocket;
begin
  PathParams := TCollections.CreateDictionary<string, string>(TIStringComparer.Ordinal);

  if ApiRequest.Method = rmUnknown then
  begin
    ApiResponse.SetResponseCode(404, 'Invalid HTTP method.');
    Exit;
  end;

  if ApiRequest.MimeType = mtUnknown then
  begin
    ApiResponse.SetResponseCode(400, 'Request mime type not supported.');
    Exit;
  end;

  if ApiResponse.MimeType = mtUnknown then
  begin
    ApiResponse.SetResponseCode(406);
    Exit;
  end;

  if ServerWebSocket.DetectLoop(FWebSockets, ApiRequest, ApiResponse, WebSocket) then
  begin
    //This will loop until the websocket is active. then it can exit.
    WebSocket.ReadLoop();
    Exit;
  end
  else if FWebServer.Process(ApiRequest, ApiResponse) then
  begin
    ApiResponse.SetResponseCode(200, 'OK');
    Exit;
  end;

  if not TryGetEndPoint(ApiRequest, EndPoint) then
  begin
    ApiResponse.SetResponseCode(404, 'Endpoint not found.');
    Exit;
  end;

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
        Step: string;
        PApiepFunc: TRequestMiddlewareFunc;
        PostStepProc: TResponseMiddlewareProc;
        ResponseCode: Integer;
        ResponseText: string;
        Result: TValue;
      begin
        if not TrySetPathParams(EndPoint, ApiRequest.URI, PathParams) then
        begin
          ApiResponse.SetResponseCode(400, 'Bad number or type of path parameters.');
          Exit;
        end;

        if not TrySetMethodParams(ApiRequest, PathParams, EndPoint, Params) then
        begin
          ApiResponse.SetResponseCode(400, 'Bad number or type of parameters.');
          Exit;
        end;

        try
          for Step in Endpoint.PreProcessPipelineSteps do
            if not FRequestMiddlewares.TryGetValue(Step, PApiepFunc) then
              raise EFidoApiException.Create('RequestMiddleware ' + Step + ' not found.')
            else if not PApiepFunc(ApiRequest, ResponseCode, ResponseText) then
            begin
              ApiResponse.SetResponseCode(ResponseCode, ResponseText);
              Exit;
            end;

          Result := Method.Invoke(EndPoint.Instance, Params);
          UpdateResponse(Method, Result, ApiResponse, EndPoint, Params);

          for Step in Endpoint.PostProcessPipelineSteps do
          begin
            if not FResponseMiddlewares.TryGetValue(Step, PostStepProc) then
              raise EFidoApiException.Create('ResponseMiddleware ' + Step + ' not found.');
            PostStepProc(ApiRequest, ApiResponse)
          end;

        except
          on E: EApiServer400 do
            ApiResponse.SetResponseCode(400, 'Bad request.');
          on E: EApiServer401 do
            ApiResponse.SetResponseCode(401, 'Unhautorized.');
          on E: EApiServer403 do
            ApiResponse.SetResponseCode(403, 'Forbidden.');
          on E: EApiServer404 do
            ApiResponse.SetResponseCode(404, 'Not found.');
          on E: EApiServer409 do
            ApiResponse.SetResponseCode(409, 'Conflict.');
          on E: EApiServer500 do
            ApiResponse.SetResponseCode(500, 'Internal server error.');
          else
            raise;
          Exit;
        end;

        if Result.IsObject then
          Result.AsObject.Free;
        for Param in Params do
          if Param.IsObject then
            Param.AsObject.Free;
        Exit;
      end);
end;

procedure TIndyApiServer.OnHTTPCommandEvent(
  Context: TIdContext;
  RequestInfo: TIdHTTPRequestInfo;
  ResponseInfo: TIdHTTPResponseInfo);
var
  ApiRequest: IHttpRequest;
  ApiResponse: IHttpResponse;
begin
  if RequestInfo.URI.Equals('/favicon.ico') then
    Exit;

  ApiRequest := FApiRequestFactory(RequestInfo);
  ApiResponse := FApiResponseFactory(Context, RequestInfo, ResponseInfo);

  ProcessCommand(ApiRequest, ApiResponse);
end;

procedure TIndyApiServer.OnParseAuthentication(
  Context: TIdContext;
  const AuthType: string;
  const AuthData: string;
  var Username: string;
  var Password: string;
  var Handled: Boolean);
begin
  Handled := True;
end;

procedure TIndyApiServer.OnQuerySSLPort(
  APort: Word;
  var VUseSSL: Boolean);
begin
  VUseSSL := (APort = SSL_PORT);
end;

function TIndyApiServer.OnVerifyPeer(
  Certificate: TIdX509;
  AOk: Boolean;
  ADepth, AError: Integer): Boolean;
begin
  result := AOk;
end;

procedure TIndyApiServer.RegisterResource(const Resource: TObject);
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
      PreProcessPipelineSteps: IList<string>;
      PostProcessPipelineSteps: IList<string>;
    begin
      ResponseCode := 200;
      ResponseText := 'OK';

      ApiMethod := rmUnknown;
      MethodPath := '';
      Parameters := TCollections.CreateList<TEndPointParameter>;
      PreProcessPipelineSteps := TCollections.CreateList<string>;
      PostProcessPipelineSteps := TCollections.CreateList<string>;

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
            PreProcessPipelineSteps.Add((Attribute as RequestMiddlewareAttribute).StepName)
          else if Attribute is ResponseMiddlewareAttribute then
            PostProcessPipelineSteps.Add((Attribute as ResponseMiddlewareAttribute).StepName);
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
            PostProcessPipelineSteps);
      end;
    end);
end;

procedure TIndyApiServer.RegisterWebSocket(const WebSocketClass: TClass);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Attribute: TCustomAttribute;
  Path: string;
begin
  if not WebSocketClass.InheritsFrom(TLoopServerWebSocket) then
    Exit;
  Context := TRttiContext.Create;
  RttiType := Context.GetType(WebSocketClass);

  if TCollections.CreateList<TCustomAttribute>(RttiType.GetAttributes).TryGetFirst(
      Attribute,
      function(const Attribute: TCustomAttribute): Boolean
      begin
        Result := Attribute is WebSocketPathAttribute;
      end) then
    Path := (Attribute as WebSocketPathAttribute).Path;

  FLock.BeginWrite;
  try
    FWebSockets.Add(TranslatePathToRegEx(Path), WebSocketClass);
  finally
    FLock.EndWrite;
  end;
end;

procedure TIndyApiServer.RegisterRequestMiddleware(
  const Name: string;
  const Step: TRequestMiddlewareFunc);
begin
  FRequestMiddlewares.Add(Name, Step);
end;

procedure TIndyApiServer.RegisterResponseMiddleware(
  const Name: string;
  const Step: TResponseMiddlewareProc);
begin
  FResponseMiddlewares.Add(Name, Step);
end;

function TIndyApiServer.TranslatePathToRegEx(Path: string): string;
begin
  Result := StringReplace(Path, '/', '\/', [rfReplaceAll]);
  with TRegEx.Matches(Result, '{[\s\S][^{]+}').GetEnumerator do
  begin
    while MoveNext do
      Result := StringReplace(Result, GetCurrent.Value, '[\s\S]+', []);
    Free;
  end;
  Result := Result + '$';
end;

procedure TIndyApiServer.SetActive(const Value: Boolean);
begin
  FHttpServer.Active := Value;
end;

end.
