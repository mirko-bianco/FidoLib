unit Fido.Redis.EventsDriven.Producer.QueueQueuePubSub.Test;

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
  Fido.EventsDriven.Producer.Intf,

  Fido.Redis.EventsDriven.Producer.QueuePubSub,
  Fido.Redis.Client.Intf;

type
  [TestFixture]
  TRedisEventsDrivenProducerQueuePubSubTests = class
  public
    [Test]
    procedure PushPushesEncodedData;

    [Test]
    procedure PushReturnsFalseWhenLPUSHFails;

    [Test]
    procedure PushReturnsFalseWhenPUBLISHFails;
  end;

implementation

procedure TRedisEventsDrivenProducerQueuePubSubTests.PushPushesEncodedData;
var
  Client: Mock<IFidoRedisClient>;
  Producer: IEventsDrivenProducer;
  Key: string;
  Payload: string;
  EncodedPayload: string;
  Result: Boolean;
begin
  Key := MockUtils.SomeString;
  Payload := MockUtils.SomeString;
  EncodedPayload := TNetEncoding.Base64.Encode(Payload);

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Integer>(1).When.LPUSH(Arg.IsAny<string>, Arg.IsIn<string>([EncodedPayload]));
  Client.Setup.Returns<Integer>(1).When.PUBLISH(Arg.IsIn<string>([Key]), Arg.IsAny<string>);

  Producer := TRedisQueuePubSubEventsDrivenProducer.Create(Client);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := Producer.Push(Key, Payload);
    end);

  Assert.AreEqual(True, Result);
  Client.Received(Times.Once).LPUSH(Arg.IsAny<string>, Arg.IsIn<string>([EncodedPayload]));
  Client.Received(Times.Never).LPUSH(Arg.IsAny<string>, Arg.IsNotIn<string>([EncodedPayload]));
  Client.Received(Times.Once).PUBLISH(Arg.IsIn<string>([Key]), Arg.IsAny<string>);
  Client.Received(Times.Never).PUBLISH(Arg.IsNotIn<string>([Key]), Arg.IsAny<string>);
end;

procedure TRedisEventsDrivenProducerQueuePubSubTests.PushReturnsFalseWhenLPUSHFails;
var
  Client: Mock<IFidoRedisClient>;
  Producer: IEventsDrivenProducer;
  Key: string;
  Payload: string;
  EncodedPayload: string;
  Result: Boolean;
begin
  Key := MockUtils.SomeString;
  Payload := MockUtils.SomeString;
  EncodedPayload := TNetEncoding.Base64.Encode(Payload);

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Integer>(0).When.LPUSH(Arg.IsAny<string>, Arg.IsIn<string>([EncodedPayload]));
  Client.Setup.Returns<Integer>(1).When.PUBLISH(Arg.IsIn<string>([Key]), Arg.IsAny<string>);

  Producer := TRedisQueuePubSubEventsDrivenProducer.Create(Client);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := Producer.Push(Key, Payload);
    end);

  Assert.AreEqual(False, Result);
  Client.Received(Times.Exactly(1)).LPUSH(Arg.IsAny<string>, Arg.IsIn<string>([EncodedPayload]));
  Client.Received(Times.Never).LPUSH(Arg.IsAny<string>, Arg.IsNotIn<string>([EncodedPayload]));
  Client.Received(Times.Never).PUBLISH(Arg.IsAny<string>, Arg.IsAny<string>);
end;

procedure TRedisEventsDrivenProducerQueuePubSubTests.PushReturnsFalseWhenPUBLISHFails;
var
  Client: Mock<IFidoRedisClient>;
  Producer: IEventsDrivenProducer;
  Key: string;
  Payload: string;
  EncodedPayload: string;
  Result: Boolean;
begin
  Key := MockUtils.SomeString;
  Payload := MockUtils.SomeString;
  EncodedPayload := TNetEncoding.Base64.Encode(Payload);

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Integer>(1).When.LPUSH(Arg.IsAny<string>, Arg.IsIn<string>([EncodedPayload]));
  Client.Setup.Returns<Integer>(0).When.PUBLISH(Arg.IsIn<string>([Key]), Arg.IsAny<string>);

  Producer := TRedisQueuePubSubEventsDrivenProducer.Create(Client);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := Producer.Push(Key, Payload);
    end);

  Assert.AreEqual(False, Result);
  Client.Received(Times.Once).LPUSH(Arg.IsAny<string>, Arg.IsIn<string>([EncodedPayload]));
  Client.Received(Times.Never).LPUSH(Arg.IsAny<string>, Arg.IsNotIn<string>([EncodedPayload]));
  Client.Received(Times.Once).PUBLISH(Arg.IsIn<string>([Key]), Arg.IsAny<string>);
  Client.Received(Times.Never).PUBLISH(Arg.IsNotIn<string>([Key]), Arg.IsAny<string>);
end;

initialization
  TDUnitX.RegisterTestFixture(TRedisEventsDrivenProducerQueuePubSubTests);
end.

