unit Fido.Consul.UseCases.KVStore.Get.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.NetEncoding,
  DUnitX.TestFramework,

  Spring,
  Spring.Mocking,
  Spring.Collections,

  Fido.Testing.Mock.Utils,
  Fido.JSON.Marshalling,
  Fido.Consul.UseCases.KVStore.Get,
  Fido.Api.Client.Consul.KVStore.V1.Intf,
  Fido.Consul.UseCases.KVStore.Get.Intf;

type
  [TestFixture]
  TConsulKVStoreGetKeyUseCaseTests = class
  public
    [Test]
    procedure RunReturnsEmptyStringWhenApiDoesNotReturnAnything;

    [Test]
    procedure RunReturnsEmptyStringWhenApiReturnsMoreThanOneItem;

    [Test]
    procedure RunReturnsValueWhenApiReturnsOneItem;
  end;

implementation

procedure TConsulKVStoreGetKeyUseCaseTests.RunReturnsEmptyStringWhenApiDoesNotReturnAnything;
var
  UseCase: Shared<TConsulKVStoreGetKeyUseCase>;
  Api: Mock<IConsulKVStoreApiV1>;
  ApiResponse: IReadonlyList<IKVStoreGetResponseItem>;
  Key: string;
  Result: string;
begin
  Key := MockUtils.SomeString;

  Api := Mock<IConsulKVStoreApiV1>.Create;
  Api.Setup.Returns<IReadonlyList<IKVStoreGetResponseItem>>(ApiResponse).When.Get(Key);

  UseCase := TConsulKVStoreGetKeyUseCase.Create(Api);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := UseCase.Value.Run(Key);
    end);

  Assert.AreEqual('', Result);
  Api.Received(Times.Once).Get(Key);
  Api.Received(Times.Never).Get(Arg.IsNotIn<string>(Key));
  Api.Received(Times.Never).Put(Arg.IsAny<string>, Arg.IsAny<string>);
  Api.Received(Times.Never).Delete(Arg.IsAny<string>);
  Api.Received(Times.Never).IsActive;
  Api.Received(Times.Never).GetLastStatusCode;
end;

procedure TConsulKVStoreGetKeyUseCaseTests.RunReturnsEmptyStringWhenApiReturnsMoreThanOneItem;
var
  UseCase: Shared<TConsulKVStoreGetKeyUseCase>;
  Api: Mock<IConsulKVStoreApiV1>;
  ApiResponse: IReadonlyList<IKVStoreGetResponseItem>;
  Key: string;
  Result: string;
begin
  Key := MockUtils.SomeString;
  ApiResponse := TCollections.CreateList<IKVStoreGetResponseItem>([nil, nil]).AsReadOnlyList;

  Api := Mock<IConsulKVStoreApiV1>.Create;
  Api.Setup.Returns<IReadonlyList<IKVStoreGetResponseItem>>(ApiResponse).When.Get(Key);

  UseCase := TConsulKVStoreGetKeyUseCase.Create(Api);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := UseCase.Value.Run(Key);
    end);

  Assert.AreEqual('', Result);
  Api.Received(Times.Once).Get(Key);
  Api.Received(Times.Never).Get(Arg.IsNotIn<string>(Key));
  Api.Received(Times.Never).Put(Arg.IsAny<string>, Arg.IsAny<string>);
  Api.Received(Times.Never).Delete(Arg.IsAny<string>);
  Api.Received(Times.Never).IsActive;
  Api.Received(Times.Never).GetLastStatusCode;
end;

procedure TConsulKVStoreGetKeyUseCaseTests.RunReturnsValueWhenApiReturnsOneItem;
var
  UseCase: Shared<TConsulKVStoreGetKeyUseCase>;
  Api: Mock<IConsulKVStoreApiV1>;
  ApiResponse: IReadonlyList<IKVStoreGetResponseItem>;
  Key: string;
  Result: string;
  ExpectedResult: string;
begin
  ExpectedResult := MockUtils.SomeString;
  Key := MockUtils.SomeString;
  ApiResponse := TCollections.CreateList<IKVStoreGetResponseItem>([
    JSONUnmarshaller.To<IKVStoreGetResponseItem>(Format('{"value":"%s"}', [TNetEncoding.Base64.Encode(ExpectedResult)]))]).AsReadOnlyList;

  Api := Mock<IConsulKVStoreApiV1>.Create;
  Api.Setup.Returns<IReadonlyList<IKVStoreGetResponseItem>>(ApiResponse).When.Get(Key);

  UseCase := TConsulKVStoreGetKeyUseCase.Create(Api);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := UseCase.Value.Run(Key);
    end);

  Assert.AreEqual(ExpectedResult, Result);
  Api.Received(Times.Once).Get(Key);
  Api.Received(Times.Never).Get(Arg.IsNotIn<string>(Key));
  Api.Received(Times.Never).Put(Arg.IsAny<string>, Arg.IsAny<string>);
  Api.Received(Times.Never).Delete(Arg.IsAny<string>);
  Api.Received(Times.Never).IsActive;
  Api.Received(Times.Never).GetLastStatusCode;
end;

initialization
  TDUnitX.RegisterTestFixture(TConsulKVStoreGetKeyUseCaseTests);
end.
