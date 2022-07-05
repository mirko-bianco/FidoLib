unit Fido.Consul.KVStore.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  DUnitX.TestFramework,

  Spring,
  Spring.Mocking,

  Fido.Functional,
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

  LGetRun: Context<string>;
begin
  DeleteKey := MockUtils.SomeString;

  GetUseCase := Mock<IConsulKVStoreGetKeyUseCase>.Create;
  PutUseCase := Mock<IConsulKVStorePutKeyUseCase>.Create;
  DeleteUseCase := Mock<IConsulKVStoreDeleteKeyUseCase>.Create;
  DeleteUseCase.Setup.Returns<Context<Boolean>>(Context<Boolean>.New(True)).When.Run(DeleteKey);

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
  LGetRun := GetUseCase.Received(Times.Never).Run(Arg.IsAny<string>, Arg.IsAny<Cardinal>);
  PutUseCase.Received(Times.Never).Run(Arg.IsAny<string>, Arg.IsAny<string>, Arg.IsAny<Cardinal>);
  DeleteUseCase.Received(Times.Once).Run(DeleteKey, INFINITE);
  DeleteUseCase.Received(Times.Never).Run(Arg.IsNotIn<string>([DeleteKey]), Arg.IsNotIn<Cardinal>([INFINITE]));
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
  GetUseCase.Setup.Returns<Context<string>>(Context<string>.New(GetValue)).When.Run(GetKey);
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
  GetUseCase.Received(Times.Once).Run(GetKey, INFINITE);
  GetUseCase.Received(Times.Never).Run(Arg.IsNotIn<string>([GetKey]), Arg.IsNotIn<Cardinal>([INFINITE]));
  PutUseCase.Received(Times.Never).Run(Arg.IsAny<string>, Arg.IsAny<string>, Arg.IsAny<Cardinal>);
  DeleteUseCase.Received(Times.Never).Run(Arg.IsAny<string>, Arg.IsAny<Cardinal>);
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
  PutUseCase.Setup.Returns<Context<Boolean>>(Context<Boolean>.New(True)).When.Run(PutKey, PutValue);
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
  GetUseCase.Received(Times.Never).Run(Arg.IsAny<string>, Arg.IsAny<Cardinal>);
  PutUseCase.Received(Times.Once).Run(PutKey, PutValue, INFINITE);
  PutUseCase.Received(Times.Never).Run(Arg.IsNotIn<string>([PutKey]), Arg.IsNotIn<string>([PutValue]), Arg.IsNotIn<Cardinal>([INFINITE]));
  DeleteUseCase.Received(Times.Never).Run(Arg.IsAny<string>, Arg.IsAny<Cardinal>);
end;

initialization
  TDUnitX.RegisterTestFixture(TConsulKVStoreTests);
end.
