unit Fido.Redis.EventsDriven.Consumer.Queue.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.NetEncoding,
  DUnitX.TestFramework,

  Spring,
  Spring.Mocking,

  Fido.Functional,
  Fido.Testing.Mock.Utils,
  Fido.EventsDriven.Consumer.Queue.Intf,

  Fido.Redis.EventsDriven.Consumer.Queue,
  Fido.Redis.Client.Intf;

type
  [TestFixture]
  TRedisEventsDrivenConsumerQueueTests = class
  public
    [Test]
    procedure PopReturnsTrueAndValueWhenFound;

    [Test]
    procedure PopReturnsFalseWhenNotFound;

    [Test]
    procedure PushBackPushesEndodedData;
  end;

implementation

procedure TRedisEventsDrivenConsumerQueueTests.PopReturnsTrueAndValueWhenFound;
var
  Client: Mock<IFidoRedisClient>;
  Consumer: IQueueEventsDrivenConsumer<string>;
  Key: string;
  Payload: string;
  Result: Nullable<string>;
  ExpectedResult: Boolean;
begin
  Key := MockUtils.SomeString;
  Payload := MockUtils.SomeString;
  ExpectedResult := True;

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Context<Nullable<string>>>(Context<Nullable<string>>.New(TNetEncoding.Base64.Encode(Payload))).When.RPOP(Key);

  Consumer := TRedisQueueEventsDrivenConsumer.Create(Client);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := Consumer.Pop(Key);
    end);

  Assert.AreEqual(ExpectedResult, Result.HasValue);
  Assert.AreEqual(Payload, Result.Value);
  Client.Received(Times.Once).RPOP(Key, INFINITE);
  Client.Received(Times.Never).RPOP(Arg.IsNotIn<string>(Key), Arg.IsNotIn<Cardinal>([INFINITE]));
end;

procedure TRedisEventsDrivenConsumerQueueTests.PopReturnsFalseWhenNotFound;
var
  Client: Mock<IFidoRedisClient>;
  Consumer: IQueueEventsDrivenConsumer<string>;
  Key: string;
  Payload: string;
  Result: Nullable<string>;
  ExpectedResult: Boolean;
  NullableString: Nullable<string>;
begin
  Key := MockUtils.SomeString;
  Payload := MockUtils.SomeString;
  ExpectedResult := False;

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Context<Nullable<string>>>(Context<Nullable<string>>.New(NullableString)).When.RPOP(Key);

  Consumer := TRedisQueueEventsDrivenConsumer.Create(Client);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := Consumer.Pop(Key);
    end);

  Assert.AreEqual(ExpectedResult, Result.HasValue);
  Client.Received(Times.Once).RPOP(Key, INFINITE);
  Client.Received(Times.Never).RPOP(Arg.IsNotIn<string>(Key), Arg.IsNotIn<Cardinal>([INFINITE]));
end;

procedure TRedisEventsDrivenConsumerQueueTests.PushBackPushesEndodedData;
var
  Client: Mock<IFidoRedisClient>;
  Consumer: IQueueEventsDrivenConsumer<string>;
  Key: string;
  Payload: string;
  EncodedPayload: string;
begin
  Key := MockUtils.SomeString;
  Payload := MockUtils.SomeString;
  EncodedPayload := TNetEncoding.Base64.Encode(Payload);

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Context<Integer>>(Context<Integer>.New(1)).When.LPUSH(Key, EncodedPayload, INFINITE);

  Consumer := TRedisQueueEventsDrivenConsumer.Create(Client);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Consumer.PushBack(Key, Payload).Value;
    end);

  Client.Received(Times.Once).LPUSH(Key, EncodedPayload, INFINITE);
  Client.Received(Times.Never).LPUSH(Arg.IsNotIn<string>(Key), Arg.IsNotIn<string>([EncodedPayload]), Arg.IsNotIn<Cardinal>([INFINITE]));
end;

initialization
  TDUnitX.RegisterTestFixture(TRedisEventsDrivenConsumerQueueTests);
end.
