unit Fido.Redis.EventsDriven.Consumer.PubSub.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.NetEncoding,
  DUnitX.TestFramework,

  Redis.Commons,

  Spring,
  Spring.Mocking,

  Fido.Functional,
  Fido.Testing.Mock.Utils,
  Fido.EventsDriven.Consumer.PubSub.Intf,
  Fido.EventsDriven.Utils,

  Fido.Redis.EventsDriven.Consumer.PubSub,
  Fido.Redis.Client.Intf;

type
  [TestFixture]
  TRedisEventsDrivenConsumerPubSubTests = class
  public
    [Test]
    procedure SubscribeAndUnsubscribeDoNotRaiseAnyException;

    [Test]
    procedure StopDoesNotRaiseAnyException;
  end;

implementation

procedure TRedisEventsDrivenConsumerPubSubTests.SubscribeAndUnsubscribeDoNotRaiseAnyException;
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

      RedisClient := Mock<IFidoRedisClient>.Create;

      Consumer := TRedisPubSubEventsDrivenConsumer.Create(FactoryFunc);
      Consumer.Subscribe(Channel, EventName, Proc);
      Consumer.Unsubscribe(Channel, EventName);

      FactoryFunc := nil;
      Proc := nil;
    end);
end;

procedure TRedisEventsDrivenConsumerPubSubTests.StopDoesNotRaiseAnyException;
begin
  Assert.WillNotRaiseAny(
    procedure
    var
      Consumer: IPubSubEventsDrivenConsumer<string>;
      RedisClient: Mock<IFidoRedisClient>;
    begin
      RedisClient := Mock<IFidoRedisClient>.Create;

      Consumer := TRedisPubSubEventsDrivenConsumer.Create(
        function: IFidoRedisClient
        begin
          Result := RedisClient.Create;
        end);

      Consumer.Stop;
    end);
end;

initialization
  TDUnitX.RegisterTestFixture(TRedisEventsDrivenConsumerPubSubTests);
end.
