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
    procedure SubscribeDoesNotRaiseAnyException;

    [Test]
    procedure UnsubscribeDoesNotRaiseAnyException;

    [Test]
    procedure StopDoesNotRaiseAnyException;
  end;

implementation

procedure TRedisEventsDrivenConsumerQueuePubSubTests.SubscribeDoesNotRaiseAnyException;
begin
  Assert.WillNotRaiseAny(
    procedure
    var
      Consumer: IPubSubEventsDrivenConsumer<string>;
      Channel: string;
      EventName: string;
      Proc: TProc<string, string>;
      Key: string;
    begin
      Channel := MockUtils.SomeString;
      EventName := MockUtils.SomeString;
      Key := TEventsDrivenUtilities.FormatKey(Channel, EventName);
      Proc := procedure(First: string; Second: string)
        begin
        end;

      Consumer := TRedisQueuePubSubEventsDrivenConsumer.Create(
        function: IFidoRedisClient
        begin
          Result := Mock<IFidoRedisClient>.Create;
        end);

      Consumer.Subscribe(Channel, EventName, Proc);

      Proc := nil;
    end);
end;

procedure TRedisEventsDrivenConsumerQueuePubSubTests.UnsubscribeDoesNotRaiseAnyException;
begin
  Assert.WillNotRaiseAny(
    procedure
    var
      Consumer: IPubSubEventsDrivenConsumer<string>;
      Channel: string;
      EventName: string;
    begin
      Channel := MockUtils.SomeString;
      EventName := MockUtils.SomeString;

      Consumer := TRedisQueuePubSubEventsDrivenConsumer.Create(
        function: IFidoRedisClient
        begin
          Result := Mock<IFidoRedisClient>.Create;
        end);

      Consumer.Unsubscribe(Channel, EventName);
    end);
end;

procedure TRedisEventsDrivenConsumerQueuePubSubTests.StopDoesNotRaiseAnyException;
begin
  Assert.WillNotRaiseAny(
    procedure
    var
      Consumer: IPubSubEventsDrivenConsumer<string>;
    begin
      Consumer := TRedisQueuePubSubEventsDrivenConsumer.Create(
        function: IFidoRedisClient
        begin
          Result := Mock<IFidoRedisClient>.Create;
        end);

      Consumer.Stop;
    end);
end;

initialization
  TDUnitX.RegisterTestFixture(TRedisEventsDrivenConsumerQueuePubSubTests);
end.
