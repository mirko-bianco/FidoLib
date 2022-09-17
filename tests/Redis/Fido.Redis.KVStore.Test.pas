unit Fido.Redis.KVStore.Test;

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

  Fido.Redis.KVStore,
  Fido.Redis.Client.Intf;

type
  [TestFixture]
  TRedisKVStoreTests = class
  public
    [Test]
    procedure GetReturnsTheCorrectValueIfExists;

    [Test]
    procedure GetReturnsEmptyStringIfDoesNotExist;

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
  Client.Setup.Returns<Context<Integer>>(Context<Integer>.New(1)).When.DEL(Format('%s%s', [KEYPREFIX, Key]));

  KVStore := TRedisKVStore.Create(Client, KEYPREFIX);

  Assert.WillNotRaiseAny(
    procedure
    var
      LValue: Context<Boolean>;
    begin
      LValue := KVStore.Delete(Key);
      Result := LValue;
    end);

  Assert.AreEqual(ExpectedResult, Result);
  Client.Received(Times.Once).DEL(Format('%s%s', [KEYPREFIX, Key]), INFINITE);
  Client.Received(Times.Never).Del(Arg.IsNotIn<string>(Format('%s%s', [KEYPREFIX, Key])), Arg.IsNotIn<Cardinal>([INFINITE]));
end;

procedure TRedisKVStoreTests.GetReturnsEmptyStringIfDoesNotExist;
var
  Client: Mock<IFidoRedisClient>;
  KVStore: IKVStore;
  Key: string;
  Result: string;
  ExpectedResult: Nullable<string>;
begin
  Key := MockUtils.SomeString;

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Context<Nullable<string>>>(Context<Nullable<string>>.New(ExpectedResult)).When.GET(Format('%s%s', [KEYPREFIX, Key]));

  KVStore := TRedisKVStore.Create(Client, KEYPREFIX);

  Assert.WillNotRaiseAny(
    procedure
    var
      LValue: Context<string>;
    begin
      LValue := KVStore.Get(Key);
      Result := LValue;
    end);

  Assert.AreEqual('', Result);
  Client.Received(Times.Once).GET(Format('%s%s', [KEYPREFIX, Key]), INFINITE);
  Client.Received(Times.Never).GET(Arg.IsNotIn<string>([Format('%s%s', [KEYPREFIX, Key])]), Arg.IsNotIn<Cardinal>([INFINITE]));
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
  ExpectedResult := MockUtils.WithNullChance(0).SomeNullableString;

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Context<Nullable<string>>>(Context<Nullable<string>>.New(ExpectedResult)).When.GET(Format('%s%s', [KEYPREFIX, Key]), INFINITE);

  KVStore := TRedisKVStore.Create(Client, KEYPREFIX);

  Assert.WillNotRaiseAny(
    procedure
    var
      LValue: Context<string>;
    begin
      LValue := KVStore.Get(Key);
      Result := LValue;
    end);

  Assert.AreEqual(ExpectedResult, Result);
  Client.Received(Times.Once).GET(Format('%s%s', [KEYPREFIX, Key]), INFINITE);
  Client.Received(Times.Never).GET(Arg.IsNotIn<string>([Format('%s%s', [KEYPREFIX, Key])]), Arg.IsNotIn<Cardinal>([INFINITE]));
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
  Client.Setup.Returns<Context<Boolean>>(Context<Boolean>.New(ExpectedResult)).When.&SET(Format('%s%s', [KEYPREFIX, Key]), Value);

  KVStore := TRedisKVStore.Create(Client, KEYPREFIX);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := KVStore.Put(Key, Value);
    end);

  Assert.AreEqual(ExpectedResult, Result);
  Client.Received(Times.Once).&SET(Format('%s%s', [KEYPREFIX, Key]), Value, INFINITE);
  Client.Received(Times.Never).&SET(Arg.IsNotIn<string>([Format('%s%s', [KEYPREFIX, Key])]), Arg.IsNotIn<string>([Value]), Arg.IsNotIn<Cardinal>([INFINITE]));
end;

initialization
  TDUnitX.RegisterTestFixture(TRedisKVStoreTests);
end.
