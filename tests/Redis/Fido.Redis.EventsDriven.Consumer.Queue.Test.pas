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
  Consumer: IEventsDrivenQueueConsumer;
  Key: string;
  Payload: string;
  Result: Boolean;
  ExpectedResult: Boolean;
  ResultPayload: string;
begin
  Key := MockUtils.SomeString;
  Payload := MockUtils.SomeString;
  ExpectedResult := True;

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Nullable<string>>(TNetEncoding.Base64.Encode(Payload)).When.RPOP(Key);

  Consumer := TRedisEventsDrivenQueueConsumer.Create(Client);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := Consumer.Pop(Key, ResultPayload);
    end);

  Assert.AreEqual(ExpectedResult, Result);
  Assert.AreEqual(Payload, ResultPayload);
  Client.Received(Times.Once).RPOP(Key);
  Client.Received(Times.Never).RPOP(Arg.IsNotIn<string>(Key));
end;

procedure TRedisEventsDrivenConsumerQueueTests.PopReturnsFalseWhenNotFound;
var
  Client: Mock<IFidoRedisClient>;
  Consumer: IEventsDrivenQueueConsumer;
  Key: string;
  Payload: string;
  ResultPayload: string;
  Result: Boolean;
  ExpectedResult: Boolean;
  NullableString: Nullable<string>;
begin
  Key := MockUtils.SomeString;
  Payload := MockUtils.SomeString;
  ExpectedResult := False;

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Nullable<string>>(NullableString).When.RPOP(Key);

  Consumer := TRedisEventsDrivenQueueConsumer.Create(Client);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := Consumer.Pop(Key, ResultPayload);
    end);

  Assert.AreEqual(ExpectedResult, Result);
  Client.Received(Times.Once).RPOP(Key);
  Client.Received(Times.Never).RPOP(Arg.IsNotIn<string>(Key));
end;

procedure TRedisEventsDrivenConsumerQueueTests.PushBackPushesEndodedData;
var
  Client: Mock<IFidoRedisClient>;
  Consumer: IEventsDrivenQueueConsumer;
  Key: string;
  Payload: string;
  EncodedPayload: string;
begin
  Key := MockUtils.SomeString;
  Payload := MockUtils.SomeString;
  EncodedPayload := TNetEncoding.Base64.Encode(Payload);

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Executes.When.LPUSH(Key, EncodedPayload);

  Consumer := TRedisEventsDrivenQueueConsumer.Create(Client);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Consumer.PushBack(Key, Payload);
    end);

  Client.Received(Times.Once).LPUSH(Key, EncodedPayload);
  Client.Received(Times.Never).LPUSH(Arg.IsNotIn<string>(Key), Arg.IsNotIn<string>([EncodedPayload]));
end;

initialization
  TDUnitX.RegisterTestFixture(TRedisEventsDrivenConsumerQueueTests);
end.
