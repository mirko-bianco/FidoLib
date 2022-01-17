unit Fido.Redis.KVStore.Test;

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

  Fido.Redis.KVStore,
  Fido.Redis.Client.Intf;

type
  [TestFixture]
  TRedisKVStoreTests = class
  public
    [Test]
    procedure GetReturnsTheCorrectValueIfExists;

    [Test]
    procedure GetReturnsNullIfDoesNotExist;

    [Test]
    procedure SetWorks;

    [Test]
    procedure DelWorks;
  end;

const
  KEYPREFIX = 'PREFIX';

implementation

procedure TRedisKVStoreTests.DelWorks;
var
  Client: Mock<IFidoRedisClient>;
  KVStore: IKVStore;
  Key: string;
  Value: string;
  Result: Boolean;
  ExpectedResult: Boolean;
begin
  Key := MockUtils.SomeString;
  Value := MockUtils.SomeString;
  ExpectedResult := True;

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Integer>(1).When.DEL(Format('%s%s', [KEYPREFIX, Key]));

  KVStore := TRedisKVStore.Create(Client, KEYPREFIX);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := KVStore.Delete(Key);
    end);

  Assert.AreEqual(ExpectedResult, Result);
  Client.Received(Times.Once).DEL(Format('%s%s', [KEYPREFIX, Key]));
  Client.Received(Times.Never).Del(Arg.IsNotIn<string>(Format('%s%s', [KEYPREFIX, Key])));
end;

procedure TRedisKVStoreTests.GetReturnsNullIfDoesNotExist;
var
  Client: Mock<IFidoRedisClient>;
  KVStore: IKVStore;
  Key: string;
  Result: string;
  ExpectedResult: Nullable<string>;
begin
  Key := MockUtils.SomeString;

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Nullable<string>>(ExpectedResult).When.GET(Format('%s%s', [KEYPREFIX, Key]));

  KVStore := TRedisKVStore.Create(Client, KEYPREFIX);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := KVStore.Get(Key);
    end);

  Assert.AreEqual('', Result);
  Client.Received(Times.Once).GET(Format('%s%s', [KEYPREFIX, Key]));
  Client.Received(Times.Never).GET(Arg.IsNotIn<string>([Format('%s%s', [KEYPREFIX, Key])]));
end;

procedure TRedisKVStoreTests.GetReturnsTheCorrectValueIfExists;
var
  Client: Mock<IFidoRedisClient>;
  KVStore: IKVStore;
  Key: string;
  Result: string;
  ExpectedResult: string;
begin
  Key := MockUtils.SomeString;
  ExpectedResult := MockUtils.SomeString;

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Nullable<string>>(ExpectedResult).When.GET(Format('%s%s', [KEYPREFIX, Key]));

  KVStore := TRedisKVStore.Create(Client, KEYPREFIX);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := KVStore.Get(Key);
    end);

  Assert.AreEqual(ExpectedResult, Result);
  Client.Received(Times.Once).GET(Format('%s%s', [KEYPREFIX, Key]));
  Client.Received(Times.Never).GET(Arg.IsNotIn<string>([Format('%s%s', [KEYPREFIX, Key])]));
end;

procedure TRedisKVStoreTests.SetWorks;
var
  Client: Mock<IFidoRedisClient>;
  KVStore: IKVStore;
  Key: string;
  Value: string;
  Result: Boolean;
  ExpectedResult: Boolean;
begin
  Key := MockUtils.SomeString;
  Value := MockUtils.SomeString;
  ExpectedResult := True;

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Boolean>(ExpectedResult).When.&SET(Format('%s%s', [KEYPREFIX, Key]), Value);

  KVStore := TRedisKVStore.Create(Client, KEYPREFIX);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := KVStore.Put(Key, Value);
    end);

  Assert.AreEqual(ExpectedResult, Result);
  Client.Received(Times.Once).&SET(Format('%s%s', [KEYPREFIX, Key]), Value);
  Client.Received(Times.Never).&SET(Arg.IsNotIn<string>([Format('%s%s', [KEYPREFIX, Key])]), Arg.IsNotIn<string>([Value]));
end;

initialization
  TDUnitX.RegisterTestFixture(TRedisKVStoreTests);
end.
