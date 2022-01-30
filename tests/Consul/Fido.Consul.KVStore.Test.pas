unit Fido.Consul.KVStore.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  DUnitX.TestFramework,

  Spring,
  Spring.Mocking,

  Fido.Testing.Mock.Utils,
  Fido.KVStore.Intf,
  Fido.Consul.UseCases.KVStore.Get.Intf,
  Fido.Consul.UseCases.KVStore.Put.Intf,
  Fido.Consul.UseCases.KVStore.Delete.Intf,
  Fido.Consul.KVStore;

type
  [TestFixture]
  TConsulKVStoreTests = class
  public
    [Test]
    procedure GetDoesNotRaiseAnyException;

    [Test]
    procedure PutDoesNotRaiseAnyException;

    [Test]
    procedure DeleteDoesNotRaiseAnyException;
  end;

implementation

procedure TConsulKVStoreTests.DeleteDoesNotRaiseAnyException;
var
  KVStore: Shared<TConsulKVStore>;
  GetUseCase: Mock<IConsulKVStoreGetKeyUseCase>;
  PutUseCase: Mock<IConsulKVStorePutKeyUseCase>;
  DeleteUseCase: Mock<IConsulKVStoreDeleteKeyUseCase>;
  DeleteKey: string;
  Result: Boolean;
begin
  DeleteKey := MockUtils.SomeString;

  GetUseCase := Mock<IConsulKVStoreGetKeyUseCase>.Create;
  PutUseCase := Mock<IConsulKVStorePutKeyUseCase>.Create;
  DeleteUseCase := Mock<IConsulKVStoreDeleteKeyUseCase>.Create;
  DeleteUseCase.Setup.Returns<Boolean>(True).When.Run(DeleteKey);

  KVStore := TConsulKVStore.Create(
    GetUseCase,
    PutUseCase,
    DeleteUseCase);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := KVStore.Value.Delete(DeleteKey);
    end);

  Assert.AreEqual(True, Result);
  GetUseCase.Received(Times.Never).Run(Arg.IsAny<string>);
  PutUseCase.Received(Times.Never).Run(Arg.IsAny<string>, Arg.IsAny<string>);
  DeleteUseCase.Received(Times.Once).Run(DeleteKey);
  DeleteUseCase.Received(Times.Never).Run(Arg.IsNotIn<string>([DeleteKey]));
end;

procedure TConsulKVStoreTests.GetDoesNotRaiseAnyException;
var
  KVStore: Shared<TConsulKVStore>;
  GetUseCase: Mock<IConsulKVStoreGetKeyUseCase>;
  PutUseCase: Mock<IConsulKVStorePutKeyUseCase>;
  DeleteUseCase: Mock<IConsulKVStoreDeleteKeyUseCase>;
  GetKey: string;
  GetValue: string;
  Result: string;
begin
  GetKey := MockUtils.SomeString;
  GetValue := MockUtils.SomeString;

  GetUseCase := Mock<IConsulKVStoreGetKeyUseCase>.Create;
  GetUseCase.Setup.Returns<string>(GetValue).When.Run(GetKey);
  PutUseCase := Mock<IConsulKVStorePutKeyUseCase>.Create;
  DeleteUseCase := Mock<IConsulKVStoreDeleteKeyUseCase>.Create;

  KVStore := TConsulKVStore.Create(
    GetUseCase,
    PutUseCase,
    DeleteUseCase);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := KVStore.Value.Get(GetKey);
    end);

  Assert.AreEqual(GetValue, Result);
  GetUseCase.Received(Times.Once).Run(GetKey);
  GetUseCase.Received(Times.Never).Run(Arg.IsNotIn<string>([GetKey]));
  PutUseCase.Received(Times.Never).Run(Arg.IsAny<string>, Arg.IsAny<string>);
  DeleteUseCase.Received(Times.Never).Run(Arg.IsAny<string>);
end;

procedure TConsulKVStoreTests.PutDoesNotRaiseAnyException;
var
  KVStore: IKVStore;
  GetUseCase: Mock<IConsulKVStoreGetKeyUseCase>;
  PutUseCase: Mock<IConsulKVStorePutKeyUseCase>;
  DeleteUseCase: Mock<IConsulKVStoreDeleteKeyUseCase>;
  PutKey: string;
  PutValue: string;
  Result: Boolean;
begin
  PutKey := MockUtils.SomeString;
  PutValue := MockUtils.SomeString;

  GetUseCase := Mock<IConsulKVStoreGetKeyUseCase>.Create;
  PutUseCase := Mock<IConsulKVStorePutKeyUseCase>.Create;
  PutUseCase.Setup.Returns<Boolean>(True).When.Run(PutKey, PutValue);
  DeleteUseCase := Mock<IConsulKVStoreDeleteKeyUseCase>.Create;

  KVStore := TConsulKVStore.Create(
    GetUseCase,
    PutUseCase,
    DeleteUseCase);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := KVStore.Put(PutKey, PutValue);
    end);

  Assert.AreEqual(True, Result);
  GetUseCase.Received(Times.Never).Run(Arg.IsAny<string>);
  PutUseCase.Received(Times.Once).Run(PutKey, PutValue);
  PutUseCase.Received(Times.Never).Run(Arg.IsNotIn<string>([PutKey]), Arg.IsNotIn<string>([PutValue]));
  DeleteUseCase.Received(Times.Never).Run(Arg.IsAny<string>);
end;

initialization
  TDUnitX.RegisterTestFixture(TConsulKVStoreTests);
end.
