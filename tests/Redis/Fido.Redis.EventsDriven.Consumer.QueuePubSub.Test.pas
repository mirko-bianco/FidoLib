unit Fido.Redis.EventsDriven.Consumer.QueuePubSub.Test;

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
  Fido.EventsDriven.Consumer.PubSub.Intf,
  Fido.EventsDriven.Utils,

  Fido.Redis.EventsDriven.Consumer.QueuePubSub,
  Fido.Redis.Client.Intf;

type
  [TestFixture]
  TRedisEventsDrivenConsumerQueuePubSubTests = class
  public
    [Test]
    procedure SubscribeAndUnsubscribeDoNotRaiseAnyException;

    [Test]
    procedure StopDoesNotRaiseAnyException;
  end;

implementation

procedure TRedisEventsDrivenConsumerQueuePubSubTests.SubscribeAndUnsubscribeDoNotRaiseAnyException;
begin
  Assert.WillNotRaiseAny(
    procedure
    var
      Consumer: IPubSubEventsDrivenConsumer<string>;
      Channel: string;
      EventName: string;
      Proc: TProc<string, string>;
      Key: string;
      RedisClient: Mock<IFidoRedisClient>;
      FactoryFunc: TFunc<IFidoRedisClient>;
    begin
      RedisClient := Mock<IFidoRedisClient>.Create;
      Channel := MockUtils.SomeString;
      EventName := MockUtils.SomeString;
      Key := TEventsDrivenUtilities.FormatKey(Channel, EventName);
      Proc := procedure(First: string; Second: string)
        begin
        end;

      FactoryFunc := function: IFidoRedisClient
        begin
          Result := RedisClient.Instance;
        end;

      Consumer := TRedisQueuePubSubEventsDrivenConsumer.Create(FactoryFunc);
      Consumer.Subscribe(Channel, EventName, Proc);
      Consumer.Unsubscribe(Channel, EventName);

      Proc := nil;
      FactoryFunc := nil;
    end);
end;

procedure TRedisEventsDrivenConsumerQueuePubSubTests.StopDoesNotRaiseAnyException;
begin
  Assert.WillNotRaiseAny(
    procedure
    var
      Consumer: IPubSubEventsDrivenConsumer<string>;
      RedisClient: Mock<IFidoRedisClient>;
    begin
      RedisClient := Mock<IFidoRedisClient>.Create;
      Consumer := TRedisQueuePubSubEventsDrivenConsumer.Create(
        function: IFidoRedisClient
        begin
          Result := RedisClient.Instance;
        end);

      Consumer.Stop;
    end);
end;

initialization
  TDUnitX.RegisterTestFixture(TRedisEventsDrivenConsumerQueuePubSubTests);
end.
