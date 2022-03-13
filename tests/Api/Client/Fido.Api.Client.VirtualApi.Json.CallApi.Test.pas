unit Fido.Api.Client.VirtualApi.Json.CallApi.Test;

interface
uses
  System.Classes,
  System.Rtti,
  System.SysUtils,
  System.DateUtils,
  DUnitX.TestFramework,
  Rest.Types,
  Data.DBXPlatform,

  Spring,
  Spring.Collections,
  Spring.Mocking,
  Spring.Mocking.Core,

  Fido.OwningObject,
  Fido.Json.Marshalling,
  Fido.Testing.Mock.Utils,

  Fido.Api.Client.Exception,
  Fido.Api.Client.VirtualApi.Intf,
  Fido.Api.Client.VirtualApi.Configuration,
  Fido.Api.Client.VirtualApi.Configuration.Intf,
  Fido.Api.Client.VirtualApi.json,
  Fido.Api.Client.VirtualApi.Attributes,
  Fido.Api.Client.VirtualApi.Call,

  Fido.Api.Client.VirtualApi.Json.Base.Test;

const
  BASEURL = 'https://tempurl.org';
  C_APIKEY = 'ApiKey';
  C_APIKEYREST = 'Api-Key';
  GETSOMETHING = '/GetSomething/';
  POSTSOMETHING = '/PostSomething/';
  RAISERRROR = '/RaiseError/';
  GETSOMETHINGHEADERPARAM = '/GetSomethingHeaderParam/';
  GETCONTENTTYPE = '/GetContentType/';
  GETAPIVERSION = '/GetApiVersion/';
  GETACCEPTHEADER = '/GetAcceptHeader/';
  SPECIALCONTENT = 'Whatever';
  GETRESTMETHOD = '/GetRestMethod/';
  RESPONSEHEADERPARAMCORRECT = '/ReponseHeaderParamCorrect/';
  RESPONSEHEADERPARAMINCORRECT = '/ReponseHeaderParamIncorrect/';
  RESPONSEHEADERPARAMNAME = 'Location';
  RESPONSEHEADERPARAMVALUE = 'AValue';
  RESPONSEHEADERPARAMCORRECTRESPONSE = 201;
  RESPONSEHEADERPARAMINCORRECTRESPONSE = 202;
  GETMULTIPLEPARAMS = '/GetMultipleParams/';
  APIVERSIONSTRING = '1.2';
  ACCEPTHEADERWITHVERSION = 'application/json' + ';version=' + APIVERSIONSTRING;

type
  ITestConfiguration = interface(IClientVirtualApiConfiguration)
    ['{9A1163F3-74B6-4614-B4BF-8B0BE0594BB4}']
    function GetApiKey: string;
  end;

  TTestConfiguration = class(TClientVirtualApiConfiguration, ITestConfiguration)
  strict private
    FApiKey: string;
  public
    constructor Create(const BaseUrl: string; const Active: Boolean; const LiveEnvironment: Boolean; const ApiKey: string);
    function GetApiKey: string;
  end;

  {$M+}
  TTestRequest = class(TOwningObject)
  strict private
    FTheRequestMethod: TRESTRequestMethod;
    FTheApiKey: string;
    FTheApiVersion: string;
    FTheContentType: string;
    FTheAcceptHeader: string;
    FAString: string;
  published
    property TheRequestMethod: TRESTRequestMethod read FTheRequestMethod write FTheRequestMethod;
    property TheApiKey: string read FTheApiKey write FTheApiKey;
    property TheApiVersion: string read FTheApiVersion write FTheApiVersion;
    property TheContentType: string read FTheContentType write FTheContentType;
    property TheAcceptHeader: string read FTheAcceptHeader write FTheAcceptHeader;
    property AString: string read FAString write FAString;
  end;
  {$M-}

  ITestResponse = interface(IInvokable)
    ['{A29767B5-EE3F-4CE0-B61A-72C07498800C}']

    function TheRequestMethod: TRESTRequestMethod;
    function TheApiKey: string;
    function TheApiVersion: string;
    function TheContentType: string;
    function TheAcceptHeader: string;
    function AString: string;
  end;

  {$M+}
  TPostSomethingRequestRequest = class(TOwningObject)
  strict private
    FAString: string;
    FANullableString: Nullable<string>;
  published
    property AString: string read FAString write FAString;
    property ANullableString: Nullable<string> read FANullableString write FANullableString;
  end;
  {$M-}

  IPostSomethingRequestResponse = interface(IInvokable)
    ['{C56049A8-5E5F-40DA-BB69-8F12BC1BD8E6}']

    function AString: string;
    function ANullableString: Nullable<string>;
  end;

  [ApiVersion(APIVERSIONSTRING)]
  ITestClientVirtualApi = interface(IClientVirtualApi)
    ['{A441E3E5-39A6-492D-A248-8D222D59EE05}']
    [Endpoint(rmGET, GETSOMETHING + '{Changed_Id}')]
    [HeaderParam(C_APIKEY)]
    function GetSomething(const [ApiParam('Changed_Id')] Id: Integer): ITestResponse;

    [Endpoint(rmGET, GETMULTIPLEPARAMS + '{Changed_Id}/{Param2}')]
    [HeaderParam(C_APIKEY)]
    function GetWithMultipleParams(const [ApiParam('Changed_Id')] Id: Integer; const Param2: string): ITestResponse;

    [Endpoint(rmPOST, POSTSOMETHING)]
    [RequestParam('Request')]
    function PostSomething(const Request: TPostSomethingRequestRequest): IPostSomethingRequestResponse;

    [Endpoint(rmPOST, RAISERRROR)]
    Procedure RaiseError;

    [Endpoint(rmGET, GETSOMETHINGHEADERPARAM)]
    [HeaderParam(C_APIKEY, C_APIKEYREST)]
    function GetSomethingHeaderParam: ITestResponse;

    [Endpoint(rmGET, GETCONTENTTYPE)]
    [Content(SPECIALCONTENT)]
    function GetContentType: ITestResponse;

    [Endpoint(rmGET, GETAPIVERSION)]
    function GetApiVersion: ITestResponse;

    [Endpoint(rmGET, GETACCEPTHEADER)]
    function GetAcceptHeader: ITestResponse;

    [Endpoint(rmPOST, GETRESTMETHOD)]
    function GetRestMethod: ITestResponse;

    [Endpoint(rmPOST, RESPONSEHEADERPARAMCORRECT)]
    [ResponseHeaderParam(RESPONSEHEADERPARAMCORRECTRESPONSE, RESPONSEHEADERPARAMNAME)]
    procedure PostResponseHeaderParamCorrect(out Location: string);

    [Endpoint(rmPOST, RESPONSEHEADERPARAMINCORRECT)]
    [ResponseHeaderParam(RESPONSEHEADERPARAMINCORRECTRESPONSE, RESPONSEHEADERPARAMNAME)]
    procedure PostResponseHeaderParamIncorrect(out Location: string);
  end;

  TTestApiImplementation = class(TBaseTestClientVirtualApi<ITestClientVirtualApi, ITestConfiguration>)
  strict private
    procedure ImplementGetSomethingBehavior(const Call: TClientVirtualApiCall);
    procedure ImplementPostSomethingBehavior(const Call: TClientVirtualApiCall);
    procedure ImplementGetSomething2Behavior(const Call: TClientVirtualApiCall);
    procedure ImplementGetContentTypeBehavior(const Call: TClientVirtualApiCall);
    procedure ImplementGetApiVersionBehavior(const Call: TClientVirtualApiCall);
    procedure ImplementGetAcceptHeader(const Call: TClientVirtualApiCall);
    procedure ImplementGetRestMethodBehavior(const Call: TClientVirtualApiCall);
    procedure ImplementResponseHeaderWithCorrectCodeBehavior(const Call: TClientVirtualApiCall);
    procedure ImplementResponseHeaderWithIncorrectCodeBehavior(const Call: TClientVirtualApiCall);
  protected
    procedure CallApi(const Call: TClientVirtualApiCall); override;
  end;

  [TestFixture]
  TClientVirtualApiJsonTest = class(TObject)
  public
    [Test]
    procedure TestCallApiReturnsProperData;
    [Test]
    procedure TestCallApiThatRequiresAParamInARestFormatRetrievesTheParamFromCallParamters;
    [Test]
    procedure TestCallApiThatRequiresAHeaderParamRetrievesTheParamFromTheConfigurationIfNotAvailable;
    [Test]
    procedure TestCallApiThatRequiresAHeaderParamInARestFormatRetrievesTheParamFromTheConfigurationIfNotAvailable;
    [Test]
    procedure TestCallApiThatRequiresARequestParamRetrievesTheParamFromCallParamters;
    [Test]
    procedure TestCallApiThatRaisesAnErrorRetrievesAProperException;
    [Test]
    procedure TestCallApiThatRequiresADifferentContentTypeActuallySetsTheContentType;
    [Test]
    procedure TestCallApiUsesApiVersion;
    [Test]
    procedure TestCallApiUsesAcceptHeaderWithApiVersion;
    [Test]
    procedure TestCallApiUsesTheProperRestMethod;
    [Test]
    procedure TestCallApiThatRequiresAResponseHeaderParamReturnsTheValueWhenTheCodeIsCorrect;
    [Test]
    procedure TestCallApiThatRequiresAResponseHeaderParamReturnsTheValueWhenTheCodeIsIncorrect;
    [Test]
    procedure ConvertResponseForErrorCodeAttributeWorks;
  end;

implementation

{ TClientVirtualApiJsonTest }
procedure TClientVirtualApiJsonTest.ConvertResponseForErrorCodeAttributeWorks;
var
  Attribute: Shared<ConvertResponseForErrorCodeAttribute>;
  ErrorCode: Integer;
  ParamName: string;
begin
  ErrorCode := MockUtils.SomeInteger;
  ParamName := MockUtils.SomeString;

  Attribute := ConvertResponseForErrorCodeAttribute.Create(ErrorCode, ParamName);

  Assert.AreEqual(ErrorCode, Attribute.Value.ErrorCode);
  Assert.AreEqual(ParamName, Attribute.Value.ParamName);
end;

procedure TClientVirtualApiJsonTest.TestCallApiReturnsProperData;
var
  Response: ITestResponse;
  Conf: ITestConfiguration;
  Api: TTestApiImplementation;
  SomeApiKey: string;
begin
  SomeApiKey := MockUtils.SomeString;

  Conf := TTestConfiguration.Create(BASEURL, True, True, SomeApiKey);
  Api := TTestApiImplementation.Create(Conf);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Response := (Api as ITestClientVirtualApi).GetSomething(MockUtils.SomeInteger);
    end);
end;

procedure TClientVirtualApiJsonTest.TestCallApiThatRaisesAnErrorRetrievesAProperException;
var
  Conf: ITestConfiguration;
  Api: TTestApiImplementation;
  SomeApiKey: string;
begin
  SomeApiKey := MockUtils.SomeString;

  Conf := TTestConfiguration.Create(BASEURL, True, True, SomeApiKey);
  Api := TTestApiImplementation.Create(Conf);

  Assert.WillRaise(
    procedure
    begin
      (Api as ITestClientVirtualApi).RaiseError;
    end,
    EFidoClientApiException);
end;

procedure TClientVirtualApiJsonTest.TestCallApiThatRequiresADifferentContentTypeActuallySetsTheContentType;
var
  Response: ITestResponse;
  Conf: ITestConfiguration;
  Api: TTestApiImplementation;
  SomeApiKey: string;
begin
  SomeApiKey := MockUtils.SomeString;

  Conf := TTestConfiguration.Create(BASEURL, True, True, SomeApiKey);
  Api := TTestApiImplementation.Create(Conf);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Response := (Api as ITestClientVirtualApi).GetContentType;
    end);

  Assert.AreEqual(SPECIALCONTENT, Response.TheContentType);
end;

procedure TClientVirtualApiJsonTest.TestCallApiThatRequiresAHeaderParamInARestFormatRetrievesTheParamFromTheConfigurationIfNotAvailable;
var
  Response: ITestResponse;
  Conf: ITestConfiguration;
  Api: TTestApiImplementation;
  SomeNumber: Integer;
  SomeApiKey: string;
begin
  SomeApiKey := MockUtils.SomeString;

  Conf := TTestConfiguration.Create(BASEURL, True, True, SomeApiKey);
  Api := TTestApiImplementation.Create(Conf);

  SomeNumber := MockUtils.SomeInteger;

  Assert.WillNotRaiseAny(
    procedure
    begin
      Response := (Api as ITestClientVirtualApi).GetSomething(SomeNumber);
    end);

  Assert.AreEqual(SomeApiKey, Response.TheApiKey);
end;

procedure TClientVirtualApiJsonTest.TestCallApiThatRequiresAHeaderParamRetrievesTheParamFromTheConfigurationIfNotAvailable;
var
  Response: ITestResponse;
  Conf: ITestConfiguration;
  Api: TTestApiImplementation;
  SomeNumber: Integer;
  SomeApiKey: string;
begin
  SomeApiKey := MockUtils.SomeString;

  Conf := TTestConfiguration.Create(BASEURL, True, True, SomeApiKey);
  Api := TTestApiImplementation.Create(Conf);

  SomeNumber := MockUtils.SomeInteger;

  Assert.WillNotRaiseAny(
    procedure
    begin
      Response := (Api as ITestClientVirtualApi).GetSomething(SomeNumber);
    end);

  Assert.AreEqual(SomeApiKey, Response.TheApiKey);
end;

procedure TClientVirtualApiJsonTest.TestCallApiThatRequiresAParamInARestFormatRetrievesTheParamFromCallParamters;
var
  Response: ITestResponse;
  Conf: ITestConfiguration;
  Api: TTestApiImplementation;
  SomeNumber: Integer;
  SomeApiKey: string;
begin
  SomeApiKey := MockUtils.SomeString;

  Conf := TTestConfiguration.Create(BASEURL, True, True, SomeApiKey);
  Api := TTestApiImplementation.Create(Conf);

  SomeNumber := MockUtils.SomeInteger;

  Assert.WillNotRaiseAny(
    procedure
    begin
      Response := (Api as ITestClientVirtualApi).GetSomething(SomeNumber);
    end);

  Assert.AreEqual(IntToStr(SomeNumber), Response.AString);
end;

procedure TClientVirtualApiJsonTest.TestCallApiThatRequiresARequestParamRetrievesTheParamFromCallParamters;
var
  Request: Shared<TPostSomethingRequestRequest>;
  Response: IPostSomethingRequestResponse;
  Conf: ITestConfiguration;
  Api: TTestApiImplementation;
  SomeApiKey: string;
begin
  SomeApiKey := MockUtils.SomeString;

  Conf := TTestConfiguration.Create(BASEURL, True, True, SomeApiKey);
  Api := TTestApiImplementation.Create(Conf);

  Request := MockUtils.SomeObject<TPostSomethingRequestRequest>;

  Assert.WillNotRaiseAny(
    procedure
    begin
      Response := (Api as ITestClientVirtualApi).PostSomething(Request);
    end);

  Assert.AreEqual(Request.Value.AString, Response.AString);
  Assert.AreEqual(Request.Value.ANullableString.HasValue, Response.ANullableString.HasValue);
  if Request.Value.ANullableString.HasValue then
    Assert.AreEqual(Request.Value.ANullableString.Value, Response.ANullableString.Value);
end;

procedure TClientVirtualApiJsonTest.TestCallApiThatRequiresAResponseHeaderParamReturnsTheValueWhenTheCodeIsCorrect;
var
  Conf: ITestConfiguration;
  Api: TTestApiImplementation;
  SomeApiKey: string;
  TestValue: string;
begin
  SomeApiKey := MockUtils.SomeString;

  Conf := TTestConfiguration.Create(BASEURL, True, True, SomeApiKey);
  Api := TTestApiImplementation.Create(Conf);

  Assert.WillNotRaiseAny(
    procedure
    begin
      (Api as ITestClientVirtualApi).PostResponseHeaderParamCorrect(TestValue);
    end);

  Assert.AreEqual(RESPONSEHEADERPARAMVALUE, TestValue);
end;

procedure TClientVirtualApiJsonTest.TestCallApiThatRequiresAResponseHeaderParamReturnsTheValueWhenTheCodeIsIncorrect;
var
  Conf: ITestConfiguration;
  Api: TTestApiImplementation;
  SomeApiKey: string;
  TestValue: string;
begin
  SomeApiKey := MockUtils.SomeString;

  Conf := TTestConfiguration.Create(BASEURL, True, True, SomeApiKey);
  Api := TTestApiImplementation.Create(Conf);

  Assert.WillNotRaiseAny(
    procedure
    begin
      (Api as ITestClientVirtualApi).PostResponseHeaderParamIncorrect(TestValue);
    end);

  Assert.AreEqual('', TestValue);
end;

procedure TClientVirtualApiJsonTest.TestCallApiUsesAcceptHeaderWithApiVersion;
var
  Response: ITestResponse;
  Conf: ITestConfiguration;
  Api: TTestApiImplementation;
  SomeApiKey: string;
  Header: string;
begin
  SomeApiKey := MockUtils.SomeString;

  Conf := TTestConfiguration.Create(BASEURL, True, True, SomeApiKey);
  Api := TTestApiImplementation.Create(Conf);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Response := (Api as ITestClientVirtualApi).GetAcceptHeader;
    end);

  Header := Response.TheAcceptHeader;

  Assert.AreEqual(ACCEPTHEADERWITHVERSION, Header);
end;

procedure TClientVirtualApiJsonTest.TestCallApiUsesApiVersion;
var
  Response: ITestResponse;
  Conf: ITestConfiguration;
  Api: TTestApiImplementation;
  SomeApiKey: string;
begin
  SomeApiKey := MockUtils.SomeString;

  Conf := TTestConfiguration.Create(BASEURL, True, True, SomeApiKey);
  Api := TTestApiImplementation.Create(Conf);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Response := (Api as ITestClientVirtualApi).GetApiVersion;
    end);

  Assert.AreEqual(APIVERSIONSTRING, Response.TheApiVersion);
end;

procedure TClientVirtualApiJsonTest.TestCallApiUsesTheProperRestMethod;
var
  Response: ITestResponse;
  Conf: ITestConfiguration;
  Api: TTestApiImplementation;
  SomeApiKey: string;
begin
  SomeApiKey := MockUtils.SomeString;

  Conf := TTestConfiguration.Create(BASEURL, True, True, SomeApiKey);
  Api := TTestApiImplementation.Create(Conf);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Response := (Api as ITestClientVirtualApi).GetRestMethod;
    end);

  Assert.AreEqual(rmPOST, Response.TheRequestMethod);
end;

{ TTestConfiguration }

constructor TTestConfiguration.Create(const BaseUrl: string; const Active, LiveEnvironment: Boolean; const ApiKey: string);
begin
  inherited Create(BaseUrl, Active, LiveEnvironment);
  FApiKey := ApiKey;
end;

function TTestConfiguration.GetApiKey: string;
begin
  Result := FApiKey;
end;

{ TTestApiImplementation }

procedure TTestApiImplementation.ImplementGetSomething2Behavior(const Call: TClientVirtualApiCall);
var
  Dto: Shared<TTestRequest>;
  ApiKey: string;
begin
  Dto := TTestRequest.Create;
  if Call.TryGetParameterValue(pkHeader, C_APIKEYREST, ApiKey) then
    Dto.Value.TheApiKey := ApiKey;
  Dto.Value.AString := MockUtils.SomeString;

  Call.Finish(200, JSONMarshaller.From<TTestRequest>(Dto), nil);
end;

procedure TTestApiImplementation.ImplementGetSomethingBehavior(const Call: TClientVirtualApiCall);
var
  Something: string;
  Dto: Shared<TTestRequest>;
  ApiKey: string;
  ResponseHeaders: Shared<TStrings>;
  OutValue: string;
begin
  Something := StringReplace(Call.Url, BASEURL + GETSOMETHING, '', []);

  Dto := TTestRequest.Create;
  if Call.TryGetParameterValue(pkHeader, C_APIKEY, ApiKey) then
    Dto.Value.TheApiKey := ApiKey;
  Dto.Value.AString := Something;

  ResponseHeaders := TStringList.Create;

  Call.Finish(200, JSONMarshaller.From<TTestRequest>(Dto), ResponseHeaders);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Call.DurationInMsecs;
      Call.Parameters;
      Call.SetParameter(pkPath, 'test', 'test');
      Call.SetParameter(pkPath, 'test', '');
      Call.TryGetParameterValue(pkPath, 'test', OutValue);
    end);
end;

procedure TTestApiImplementation.ImplementGetAcceptHeader(const Call: TClientVirtualApiCall);
var
  Dto: Shared<TTestRequest>;
begin
  Dto := TTestRequest.Create;
  Dto.Value.TheAcceptHeader := GetAcceptHeaderWithApiVersion(Call.ContentType);

  Call.Finish(200, JSONMarshaller.From<TTestRequest>(Dto));
end;

procedure TTestApiImplementation.ImplementGetApiVersionBehavior(const Call: TClientVirtualApiCall);
var
  Dto: Shared<TTestRequest>;
begin
  Dto := TTestRequest.Create;
  Dto.Value.TheApiVersion := ApiVersion;

  Call.Finish(200, JSONMarshaller.From<TTestRequest>(Dto));
end;

procedure TTestApiImplementation.ImplementGetContentTypeBehavior(const Call: TClientVirtualApiCall);
var
  Dto: Shared<TTestRequest>;
begin
  Dto := TTestRequest.Create;
  Dto.Value.TheContentType := Call.ContentType;

  Call.Finish(200, JSONMarshaller.From<TTestRequest>(Dto));
end;

procedure TTestApiImplementation.ImplementGetRestMethodBehavior(const Call: TClientVirtualApiCall);
var
  Dto: Shared<TTestRequest>;
begin
  Dto := TTestRequest.Create;
  Dto.Value.TheRequestMethod := Call.ApiMethod;

  Call.Finish(200, JSONMarshaller.From<TTestRequest>(Dto));
end;

procedure TTestApiImplementation.ImplementPostSomethingBehavior(const Call: TClientVirtualApiCall);
begin
  Call.Finish(200, Call.PostBody);
end;

procedure TTestApiImplementation.ImplementResponseHeaderWithCorrectCodeBehavior(const Call: TClientVirtualApiCall);
begin
  Call.Finish(RESPONSEHEADERPARAMCORRECTRESPONSE, Call.PostBody);
  Call.ResponseHeaders.Clear;
  Call.ResponseHeaders.Values[RESPONSEHEADERPARAMNAME] := RESPONSEHEADERPARAMVALUE;
end;

procedure TTestApiImplementation.ImplementResponseHeaderWithIncorrectCodeBehavior(const Call: TClientVirtualApiCall);
begin
  Call.Finish(RESPONSEHEADERPARAMINCORRECTRESPONSE, Call.PostBody);
end;

procedure TTestApiImplementation.CallApi(const Call: TClientVirtualApiCall);
begin
  if Call.Url.StartsWith(BASEURL+GETSOMETHING) and
     (Call.ApiMethod = rmGET) then
    ImplementGetSomethingBehavior(Call)
  else if Call.Url.StartsWith(BASEURL+POSTSOMETHING) and
          (Call.ApiMethod = rmPOST) then
    ImplementPostSomethingBehavior(Call)
  else if Call.Url.StartsWith(BASEURL+GETSOMETHINGHEADERPARAM) and
          (Call.ApiMethod = rmGET) then
    ImplementGetSomething2Behavior(Call)
  else if Call.Url.StartsWith(BASEURL+GETCONTENTTYPE) and
          (Call.ApiMethod = rmGET) then
    ImplementGetContentTypeBehavior(Call)
  else if Call.Url.StartsWith(BASEURL+GETAPIVERSION) and
          (Call.ApiMethod = rmGET) then
    ImplementGetApiVersionBehavior(Call)
  else if Call.Url.StartsWith(BASEURL+GETACCEPTHEADER) and
          (Call.ApiMethod = rmGET) then
    ImplementGetAcceptHeader(Call)
  else if Call.Url.StartsWith(BASEURL+GETRESTMETHOD) and
          (Call.ApiMethod = rmPOST) then
    ImplementGetRestMethodBehavior(Call)
  else if Call.Url.StartsWith(BASEURL+RESPONSEHEADERPARAMCORRECT) and
          (Call.ApiMethod = rmPOST) then
    ImplementResponseHeaderWithCorrectCodeBehavior(Call)
  else if Call.Url.StartsWith(BASEURL+RESPONSEHEADERPARAMINCORRECT) and
          (Call.ApiMethod = rmPOST) then
    ImplementResponseHeaderWithIncorrectCodeBehavior(Call)
  else
  begin
    Call.Finish(500, 'Something really horrible happened in there...');
  end;

end;

initialization
  TDUnitX.RegisterTestFixture(TClientVirtualApiJsonTest);
end.
