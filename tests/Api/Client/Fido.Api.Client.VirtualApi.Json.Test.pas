unit Fido.Api.Client.VirtualApi.Json.Test;

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

  Fido.Testing.Mock.Utils,

  Fido.Json.Marshalling,
  Fido.OwningObject,
  Fido.Api.Client.VirtualApi.Intf,
  Fido.Api.Client.VirtualApi.Configuration,
  Fido.Api.Client.VirtualApi.Configuration.Intf,
  Fido.Api.Client.VirtualApi.json,
  Fido.Api.Client.VirtualApi.Attributes,

  Fido.Api.Client.VirtualApi.Json.Base.Test;

type
  {$M+}
  TTestDto = class(TOwningObject)
  strict private
    FSomeData: string;
  published
    property SomeData: string read FSomeData write FSomeData;
  end;
  {$M-}

  ITestDto = interface(IInvokable)
    ['{FD5E756D-8E7D-453C-9DEB-CD3E1CFB8F93}']
   function SomeData: string;
  end;

  ITestVirtualRestApi = interface(IClientVirtualApi)
    ['{A441E3E5-39A6-492D-A248-8D222D59EE05}']
    [Endpoint(rmGET, '/Something/{Id}')]
    [HeaderParam('ApiKey')]
    function GetSomething(const Id: Integer): ITestDto;
  end;

  ITestConfiguration = interface(IClientVirtualApiConfiguration)
    ['{9A1163F3-74B6-4614-B4BF-8B0BE0594BB4}']
    function GetApiKey: string;
  end;

  TTestConfiguration = class(TClientVirtualApiConfiguration, ITestConfiguration)
    function GetApiKey: string;
  end;

  [TestFixture]
  TVirtualRestApiJsonBaseBehaviorTest = class(TObject)
  public
    [Test]
    procedure TestConvertTValueToString;
    [Test]
    procedure TestConvertResponseToDto;
    [Test]
    procedure TestConvertRequestDtoToString;
  end;

implementation

{ TVirtualRestApiJsonTest }

procedure TVirtualRestApiJsonBaseBehaviorTest.TestConvertRequestDtoToString;
var
  SomeRequest: Shared<TTestDto>;
  RequestString: string;
  Conf: ITestConfiguration;
  Api: Shared<TBaseTestClientVirtualApi<ITestVirtualRestApi, ITestConfiguration>>;
begin
  Conf := TTestConfiguration.Create('', True, True);
  Api := TBaseTestClientVirtualApi<ITestVirtualRestApi, ITestConfiguration>.Create(Conf);
  SomeRequest := MockUtils.SomeObject<TTestDto>;
  RequestString := Api.Value.ExposedConvertRequestDtoToString(TValue.From<TTestDto>(SomeRequest));

  Assert.AreEqual(RequestString, JSONMarshaller.From<TTestDto>(SomeRequest));
end;

procedure TVirtualRestApiJsonBaseBehaviorTest.TestConvertResponseToDto;
var
  Response: ITestDto;
  SomeRequest: Shared<TTestDto>;
  Conf: ITestConfiguration;
  Api: Shared<TBaseTestClientVirtualApi<ITestVirtualRestApi, ITestConfiguration>>;
begin
  Conf := TTestConfiguration.Create('', True, True);
  Api := TBaseTestClientVirtualApi<ITestVirtualRestApi, ITestConfiguration>.Create(Conf);
  SomeRequest := MockUtils.SomeObject<TTestDto>;
  Response := Api.Value.ExposedConvertResponseToDto(JSONMarshaller.From<TTestDto>(SomeRequest), TypeInfo(ITestDto)).AsType<ITestDto>;

  Assert.AreEqual(SomeRequest.Value.SomeData, Response.SomeData);
end;

type
  {$M+}
  TMyObject = class
  published
    function Id: Integer;
    function Name: Nullable<string>;
  end;
  {$M-}

procedure TVirtualRestApiJsonBaseBehaviorTest.TestConvertTValueToString;
var
  SomeInt64: Int64;
  SomeInteger: Integer;
  SomeString: string;
  SomeDouble: Double;
  BooleanTrue: Boolean;
  BooleanFalse: Boolean;
  SomeGuid: TGuid;
  SomeDateTime: TDateTime;
  Conf: ITestConfiguration;
  Api: Shared<TBaseTestClientVirtualApi<ITestVirtualRestApi, ITestConfiguration>>;
  Obj: Shared<TMyObject>;
begin
  Conf := TTestConfiguration.Create('', True, True);
  Api := TBaseTestClientVirtualApi<ITestVirtualRestApi, ITestConfiguration>.Create(Conf);

  SomeInt64 := MockUtils.SomeInt64;
  SomeInteger := MockUtils.SomeInteger;
  SomeString := MockUtils.SomeString;
  SomeDouble := MockUtils.SomeExtended;
  BooleanTrue := True;
  BooleanFalse := False;
  SomeGuid := MockUtils.SomeGuid;
  SomeDateTime := MockUtils.SomeDateTime;

  Assert.AreEqual(SomeInt64.ToString, Api.Value.ExposedConvertTValueToString(TValue.From<Int64>(SomeInt64)));
  Assert.AreEqual(SomeInteger.ToString, Api.Value.ExposedConvertTValueToString(TValue.From<Integer>(SomeInteger)));
  Assert.AreEqual(SomeString, Api.Value.ExposedConvertTValueToString(TValue.From<string>(SomeString)));
  Assert.AreEqual(TDBXPlatform.JsonFloat(SomeDouble), Api.Value.ExposedConvertTValueToString(TValue.From<Double>(SomeDouble)));
  Assert.AreEqual(BoolToStr(BooleanTrue, True), Api.Value.ExposedConvertTValueToString(TValue.From<Boolean>(BooleanTrue)));
  Assert.AreEqual(BoolToStr(BooleanFalse, True), Api.Value.ExposedConvertTValueToString(TValue.From<Boolean>(BooleanFalse)));
  Assert.AreEqual(StringReplace(StringReplace(GUIDToString(SomeGuid), '{', '', []), '}', '', []), Api.Value.ExposedConvertTValueToString(TValue.From<TGuid>(SomeGuid)));
  Assert.AreEqual(DateToISO8601(SomeDateTime), Api.Value.ExposedConvertTValueToString(SomeDateTime));

  Obj := TMyObject.Create;
  Assert.AreEqual('{"Id":"1","Name":"Null"}', Api.Value.ExposedConvertTValueToString(TValue.From<TMyObject>(Obj)));
end;

{ TTestConfiguration }

function TTestConfiguration.GetApiKey: string;
begin
  Result := MockUtils.SomeString;
end;

{ TMyObject }

function TMyObject.Id: Integer;
begin
  Result := 1;
end;

function TMyObject.Name: Nullable<string>;
var
  NullNullable: Nullable<string>;
begin
  Result := NullNullable;
end;

initialization
  TDUnitX.RegisterTestFixture(TVirtualRestApiJsonBaseBehaviorTest);
end.
