unit Fido.Redis.EventsDriven.Producer.PubSub.Test;

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
  Fido.EventsDriven.Producer.Intf,

  Fido.Redis.EventsDriven.Producer.PubSub,
  Fido.Redis.Client.Intf;

type
  [TestFixture]
  TRedisEventsDrivenProducerPubSubTests = class
  public
    [Test]
    procedure PushPushesEndodedData;
  end;

implementation

procedure TRedisEventsDrivenProducerPubSubTests.PushPushesEndodedData;
var
  Client: Mock<IFidoRedisClient>;
  Producer: IEventsDrivenProducer<string>;
  Key: string;
  Payload: string;
  EncodedPayload: string;
  Result: Boolean;
begin
  Key := MockUtils.SomeString;
  Payload := MockUtils.SomeString;
  EncodedPayload := TNetEncoding.Base64.Encode(Payload);

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Returns<Context<Integer>>(Context<Integer>.New(1)).When.PUBLISH(Key, EncodedPayload, INFINITE);

  Producer := TRedisPubSubEventsDrivenProducer.Create(Client);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := Producer.Push(Key, Payload);
    end);

  Assert.AreEqual(True, Result);
  Client.Received(Times.Once).PUBLISH(Key, EncodedPayload, INFINITE);
  Client.Received(Times.Never).PUBLISH(Arg.IsNotIn<string>(Key), Arg.IsNotIn<string>([EncodedPayload]), Arg.IsNotIn<Cardinal>([INFINITE]));
end;

initialization
  TDUnitX.RegisterTestFixture(TRedisEventsDrivenProducerPubSubTests);
end.

