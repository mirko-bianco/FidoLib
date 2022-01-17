unit Fido.EventsDriven.Subscriber.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  DUnitX.TestFramework,

  Spring,
  Spring.Mocking,

  Fido.Testing.Mock.Utils,
  Fido.EventsDriven.Attributes,
  Fido.EventsDriven.Utils,
  Fido.EventsDriven.Subscriber.Intf,
  Fido.EventsDriven.Listener.Intf,
  Fido.EventsDriven.Subscriber;

type
  [TestFixture]
  TEventsDrivenSubscriberTests = class
  public
    [Test]
    procedure SubscriberWorksWithNoConsumer;

    [Test]
    procedure SubscriberWorksWithSingleEventConsumer;

    [Test]
    procedure SubscriberWorksWithMultipleEventsConsumer;

    [Test]
    procedure SubscriberWorksWithMultipleConsumers;
  end;

  TTestNoConsumer = class
    procedure TryMe(const Payload: string);
  end;

  TTestSingleEventConsumer = class
    [EventsDriven('TestChannel', 'TestEvent')]
    procedure TestMethod(const Payload: string);
  end;

  TTestMultipleEventsConsumer = class
    [EventsDriven('TestChannel2', 'TestEvent2')]
    procedure TestMethod2(const Payload: string);

    [EventsDriven('TestChannel3', 'TestEvent3')]
    procedure TestMethod3(const Payload: string);
  end;

implementation

procedure TEventsDrivenSubscriberTests.SubscriberWorksWithMultipleConsumers;
var
  Subscriber: IEventsDrivenSubscriber;
  Listener: Mock<IEventsDrivenListener>;
  TestSingleEventConsumer: TTestSingleEventConsumer;
  TestMultipleEventsConsumer: TTestMultipleEventsConsumer;
  ConsumerData1: TConsumerData;
  ConsumerData2: TConsumerData;
  ConsumerData3: TConsumerData;
begin
  TestSingleEventConsumer := TTestSingleEventConsumer.Create;
  ConsumerData1 := TConsumerData.Create(TestSingleEventConsumer, 'TestMethod');
  TestMultipleEventsConsumer := TTestMultipleEventsConsumer.Create;
  ConsumerData2 := TConsumerData.Create(TestMultipleEventsConsumer, 'TestMethod2');
  ConsumerData3 := TConsumerData.Create(TestMultipleEventsConsumer, 'TestMethod3');

  Listener := Mock<IEventsDrivenListener>.Create;
  Listener.Setup.Executes.When.SubscribeTo(Arg.IsAny<string>, Arg.IsAny<string>, Arg.IsAny<TConsumerData>);
  Listener.Setup.Executes.When.UnsubscribeFrom(Arg.IsAny<string>, Arg.IsAny<string>);

  Subscriber := TEventsDrivenSubscriber.Create(Listener);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Subscriber.RegisterConsumer(TestSingleEventConsumer);
      Subscriber.RegisterConsumer(TestMultipleEventsConsumer);
    end);
  Listener.Received(Times.Once).SubscribeTo('TestChannel', 'TestEvent', ConsumerData1);
  Listener.Received(Times.Once).SubscribeTo('TestChannel2', 'TestEvent2', ConsumerData2);
  Listener.Received(Times.Once).SubscribeTo('TestChannel3', 'TestEvent3', ConsumerData3);

  Subscriber := nil;

  Listener.Received(Times.Never).SubscribeTo(Arg.IsNotIn<string>(['TestChannel', 'TestChannel2', 'TestChannel3']), Arg.IsNotIn<string>(['TestEvent', 'TestEvent2', 'TestEvent3']), Arg.IsNotIn<TConsumerData>([ConsumerData1, ConsumerData2, ConsumerData3]));
  Listener.Received(Times.Once).UnsubscribeFrom('TestChannel', 'TestEvent');
  Listener.Received(Times.Once).UnsubscribeFrom('TestChannel2', 'TestEvent2');
  Listener.Received(Times.Once).UnsubscribeFrom('TestChannel3', 'TestEvent3');
  Listener.Received(Times.Never).UnsubscribeFrom(Arg.IsNotIn<string>(['TestChannel', 'TestChannel2', 'TestChannel3']), Arg.IsNotIn<string>(['TestEvent', 'TestEvent2', 'TestEvent3']));
end;

procedure TEventsDrivenSubscriberTests.SubscriberWorksWithMultipleEventsConsumer;
var
  Subscriber: IEventsDrivenSubscriber;
  Listener: Mock<IEventsDrivenListener>;
  TestMultipleEventsConsumer: TTestMultipleEventsConsumer;
  ConsumerData2: TConsumerData;
  ConsumerData3: TConsumerData;
begin
  TestMultipleEventsConsumer := TTestMultipleEventsConsumer.Create;
  ConsumerData2 := TConsumerData.Create(TestMultipleEventsConsumer, 'TestMethod2');
  ConsumerData3 := TConsumerData.Create(TestMultipleEventsConsumer, 'TestMethod3');

  Listener := Mock<IEventsDrivenListener>.Create;
  Listener.Setup.Executes.When.SubscribeTo(Arg.IsAny<string>, Arg.IsAny<string>, Arg.IsAny<TConsumerData>);
  Listener.Setup.Executes.When.UnsubscribeFrom(Arg.IsAny<string>, Arg.IsAny<string>);

  Subscriber := TEventsDrivenSubscriber.Create(Listener);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Subscriber.RegisterConsumer(TestMultipleEventsConsumer);
    end);

  Listener.Received(Times.Once).SubscribeTo('TestChannel2', 'TestEvent2', ConsumerData2);
  Listener.Received(Times.Once).SubscribeTo('TestChannel3', 'TestEvent3', ConsumerData3);
  Listener.Received(Times.Never).SubscribeTo(Arg.IsNotIn<string>(['TestChannel2', 'TestChannel3']), Arg.IsNotIn<string>(['TestEvent2', 'TestEvent3']), Arg.IsNotIn<TConsumerData>([ConsumerData2, ConsumerData3]));

  Subscriber := nil;

  Listener.Received(Times.Once).UnsubscribeFrom('TestChannel2', 'TestEvent2');
  Listener.Received(Times.Once).UnsubscribeFrom('TestChannel3', 'TestEvent3');
  Listener.Received(Times.Never).UnsubscribeFrom(Arg.IsNotIn<string>(['TestChannel2', 'TestChannel3']), Arg.IsNotIn<string>(['TestEvent2', 'TestEvent3']));
end;

procedure TEventsDrivenSubscriberTests.SubscriberWorksWithNoConsumer;
var
  Subscriber: IEventsDrivenSubscriber;
  Listener: Mock<IEventsDrivenListener>;
  Channel: string;
  EventName: string;
  TestSNoConsumer: TTestNoConsumer;
  ConsumerData: TConsumerData;
begin
  TestSNoConsumer := TTestNoConsumer.Create;

  Listener := Mock<IEventsDrivenListener>.Create;
  Listener.Setup.Executes.When.SubscribeTo(Arg.IsAny<string>, Arg.IsAny<string>, Arg.IsAny<TConsumerData>);
  Listener.Setup.Executes.When.UnsubscribeFrom(Arg.IsAny<string>, Arg.IsAny<string>);

  Subscriber := TEventsDrivenSubscriber.Create(Listener);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Subscriber.RegisterConsumer(TestSNoConsumer);
    end);

  Listener.Received(Times.Never).SubscribeTo(Channel, EventName, ConsumerData);

  Subscriber := nil;

  Listener.Received(Times.Never).UnsubscribeFrom(Channel, EventName);
end;

procedure TEventsDrivenSubscriberTests.SubscriberWorksWithSingleEventConsumer;
var
  Subscriber: IEventsDrivenSubscriber;
  Listener: Mock<IEventsDrivenListener>;
  Channel: string;
  EventName: string;
  TestSingleEventConsumer: TTestSingleEventConsumer;
  ConsumerData: TConsumerData;
begin
  Channel := 'TestChannel';
  EventName := 'TestEvent';
  TestSingleEventConsumer := TTestSingleEventConsumer.Create;
  ConsumerData := TConsumerData.Create(TestSingleEventConsumer, 'TestMethod');

  Listener := Mock<IEventsDrivenListener>.Create;
  Listener.Setup.Executes.When.SubscribeTo(Arg.IsAny<string>, Arg.IsAny<string>, Arg.IsAny<TConsumerData>);
  Listener.Setup.Executes.When.UnsubscribeFrom(Arg.IsAny<string>, Arg.IsAny<string>);

  Subscriber := TEventsDrivenSubscriber.Create(Listener);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Subscriber.RegisterConsumer(TestSingleEventConsumer);
    end);

  Listener.Received(Times.Once).SubscribeTo(Channel, EventName, ConsumerData);
  Listener.Received(Times.Never).SubscribeTo(Arg.IsNotIn<string>([Channel]), Arg.IsNotIn<string>([EventName]), Arg.IsNotIn<TConsumerData>([ConsumerData]));

  Subscriber := nil;

  Listener.Received(Times.Once).UnsubscribeFrom(Channel, EventName);
  Listener.Received(Times.Never).UnsubscribeFrom(Arg.IsNotIn<string>([Channel]), Arg.IsNotIn<string>([EventName]));
end;

{ TTestConsumer }

procedure TTestSingleEventConsumer.TestMethod(const Payload: string);
begin

end;

{ TTestMultipleEventsConsumer }

procedure TTestMultipleEventsConsumer.TestMethod2(const Payload: string);
begin

end;

procedure TTestMultipleEventsConsumer.TestMethod3(const Payload: string);
begin

end;

{ TTestNoConsumer }

procedure TTestNoConsumer.TryMe(const Payload: string);
begin

end;

initialization
  TDUnitX.RegisterTestFixture(TEventsDrivenSubscriberTests);
end.
