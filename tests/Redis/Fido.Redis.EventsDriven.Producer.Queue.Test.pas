unit Fido.Redis.EventsDriven.Producer.Queue.Test;

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

  Fido.Redis.EventsDriven.Producer.Queue,
  Fido.Redis.Client.Intf;

type
  [TestFixture]
  TRedisEventsDrivenProducerQueueTests = class
  public
    [Test]
    procedure PushPushesEndodedData;
  end;

implementation

procedure TRedisEventsDrivenProducerQueueTests.PushPushesEndodedData;
var
  Client: Mock<IFidoRedisClient>;
  Producer: IEventsDrivenProducer<string>;
  Key: string;
  Payload: string;
  EncodedPayload: string;
begin
  Key := MockUtils.SomeString;
  Payload := MockUtils.SomeString;
  EncodedPayload := TNetEncoding.Base64.Encode(Payload);

  Client := Mock<IFidoRedisClient>.Create;
  Client.Setup.Executes.When.LPUSH(Key, EncodedPayload);

  Producer := TRedisQueueEventsDrivenProducer.Create(Client);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Producer.Push(Key, Payload);
    end);

  Client.Received(Times.Once).LPUSH(Key, EncodedPayload);
  Client.Received(Times.Never).LPUSH(Arg.IsNotIn<string>(Key), Arg.IsNotIn<string>([EncodedPayload]));
end;

initialization
  TDUnitX.RegisterTestFixture(TRedisEventsDrivenProducerQueueTests);
end.
