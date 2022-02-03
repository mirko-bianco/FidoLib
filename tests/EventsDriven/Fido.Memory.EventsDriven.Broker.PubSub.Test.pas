unit Fido.Memory.EventsDriven.Broker.PubSub.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  DUnitX.TestFramework,

  Spring,
  Spring.Mocking,

  Fido.Testing.Mock.Utils,
  Fido.EventsDriven.Broker.PubSub.Intf,
  Fido.Memory.EventsDriven.Broker.PubSub,
  Fido.EventsDriven.Utils,
  Fido.EventsDriven.Consumer.PubSub.Intf;

type
  IPubSubEventsDrivenBroker = IPubSubEventsDrivenBroker<string>;
  IPubSubEventsDrivenConsumer = IPubSubEventsDrivenConsumer<string>;

  [TestFixture]
  TMemoryPubSubEventsDrivenBrokerTests = class
  public
    [Test]
    procedure PushWorks;

    [Test]
    procedure StopWorks;

    [Test]
    procedure SubscribeAndUnsubscribeWorks;
  end;

implementation

procedure TMemoryPubSubEventsDrivenBrokerTests.PushWorks;
var
  Consumer: Mock<IPubSubEventsDrivenConsumer>;
  Broker: TMemoryPubSubEventsDrivenBroker<string>;
  OnNotify1: TProc<string, string>;
  OnNotify2: TProc<string, string>;
  OnNotify3: TProc<string, string>;
  Event1Payload: string;
  Event2Payload: string;
  Event1ExpectedPayload: string;
  Event2ExpectedPayload: string;
  Event1Count: Integer;
  Event2Count: Integer;
  Event3Count: Integer;
begin
  Event1Count := 0;
  Event2Count := 0;
  Event3Count := 0;

  OnNotify1 := procedure(Key: string; Payload: string)
    begin
      Event1Payload := Payload;
      Inc(Event1Count);
    end;
  OnNotify2 := procedure(Key: string; Payload: string)
    begin
      Event2Payload := Payload;
      Inc(Event2Count);
    end;
  OnNotify3 := procedure(Key: string; Payload: string)
    begin
      Inc(Event3Count);
    end;

  Consumer := Mock<IPubSubEventsDrivenConsumer>.Create;
  Broker := TMemoryPubSubEventsDrivenBroker<string>.Create;
  try
    Event1ExpectedPayload := MockUtils.SomeString;
    Event2ExpectedPayload := MockUtils.SomeString;

    Assert.WillNotRaiseAny(
      procedure
      begin
        Broker.Subscribe(Consumer, TEventsDrivenUtilities.FormatKey('Channel', 'Event1'), OnNotify1);
        Broker.Subscribe(Consumer, TEventsDrivenUtilities.FormatKey('Channel', 'Event2'), OnNotify2);
        Broker.Subscribe(Consumer, TEventsDrivenUtilities.FormatKey('Channel', 'Event3'), OnNotify3);
        Broker.Push(TEventsDrivenUtilities.FormatKey('Channel', 'Event1'), Event1ExpectedPayload);
        Broker.Push(TEventsDrivenUtilities.FormatKey('Channel', 'Event2'), Event2ExpectedPayload);
      end);

    Assert.AreEqual(Event1ExpectedPayload, Event1Payload);
    Assert.AreEqual(1, Event1Count);
    Assert.AreEqual(Event2ExpectedPayload, Event2Payload);
    Assert.AreEqual(1, Event2Count);
    Assert.AreEqual(0, Event3Count);
  finally
    Broker.Free;
  end;

  OnNotify1 := nil;
  OnNotify2 := nil;
  OnNotify3 := nil;
end;

procedure TMemoryPubSubEventsDrivenBrokerTests.StopWorks;
var
  Consumer: Mock<IPubSubEventsDrivenConsumer>;
  Consumer2: Mock<IPubSubEventsDrivenConsumer>;
  Broker: TMemoryPubSubEventsDrivenBroker<string>;
  OnNotify1: TProc<string, string>;
  OnNotify2: TProc<string, string>;
  OnNotify3: TProc<string, string>;
  Event1Payload: string;
  Event2Payload: string;
  Event1ExpectedPayload: string;
  Event2ExpectedPayload: string;
  Event1Count: Integer;
  Event2Count: Integer;
  Event3Count: Integer;
begin
  Event1Count := 0;
  Event2Count := 0;
  Event3Count := 0;

  OnNotify1 := procedure(Key: string; Payload: string)
    begin
      Event1Payload := Payload;
      Inc(Event1Count);
    end;
  OnNotify2 := procedure(Key: string; Payload: string)
    begin
      Event2Payload := Payload;
      Inc(Event2Count);
    end;
  OnNotify3 := procedure(Key: string; Payload: string)
    begin
      Inc(Event3Count);
    end;

  Consumer := Mock<IPubSubEventsDrivenConsumer>.Create;
  Consumer2 := Mock<IPubSubEventsDrivenConsumer>.Create;
  Broker := TMemoryPubSubEventsDrivenBroker<string>.Create;
  try
    Event1ExpectedPayload := MockUtils.SomeString;
    Event2ExpectedPayload := MockUtils.SomeString;

    Assert.WillNotRaiseAny(
      procedure
      begin
        Broker.Subscribe(Consumer, TEventsDrivenUtilities.FormatKey('Channel', 'Event1'), OnNotify1);
        Broker.Subscribe(Consumer2, TEventsDrivenUtilities.FormatKey('Channel', 'Event2'), OnNotify2);
        Broker.Subscribe(Consumer2, TEventsDrivenUtilities.FormatKey('Channel', 'Event3'), OnNotify3);
        Broker.Push(TEventsDrivenUtilities.FormatKey('Channel', 'Event1'), Event1ExpectedPayload);
        Broker.Push(TEventsDrivenUtilities.FormatKey('Channel', 'Event2'), Event2ExpectedPayload);
        Broker.Stop(Consumer2);
        Broker.Push(TEventsDrivenUtilities.FormatKey('Channel', 'Event2'), Event2ExpectedPayload);
        Broker.Push(TEventsDrivenUtilities.FormatKey('Channel', 'Event3'), Event2ExpectedPayload);
      end);

    Assert.AreEqual(Event1ExpectedPayload, Event1Payload);
    Assert.AreEqual(1, Event1Count);
    Assert.AreEqual(Event2ExpectedPayload, Event2Payload);
    Assert.AreEqual(1, Event2Count);
    Assert.AreEqual(0, Event3Count);
  finally
    Broker.Free;
  end;

  OnNotify1 := nil;
  OnNotify2 := nil;
  OnNotify3 := nil;
end;

procedure TMemoryPubSubEventsDrivenBrokerTests.SubscribeAndUnsubscribeWorks;
var
  Consumer: Mock<IPubSubEventsDrivenConsumer>;
  Consumer2: Mock<IPubSubEventsDrivenConsumer>;
  Broker: TMemoryPubSubEventsDrivenBroker<string>;
  OnNotify1: TProc<string, string>;
  OnNotify2: TProc<string, string>;
  OnNotify3: TProc<string, string>;
  Event1Payload: string;
  Event2Payload: string;
  Event1ExpectedPayload: string;
  Event2ExpectedPayload: string;
  Event1Count: Integer;
  Event2Count: Integer;
  Event3Count: Integer;
begin
  Event1Count := 0;
  Event2Count := 0;
  Event3Count := 0;

  OnNotify1 := procedure(Key: string; Payload: string)
    begin
      Event1Payload := Payload;
      Inc(Event1Count);
    end;
  OnNotify2 := procedure(Key: string; Payload: string)
    begin
      Event2Payload := Payload;
      Inc(Event2Count);
    end;
  OnNotify3 := procedure(Key: string; Payload: string)
    begin
      Inc(Event3Count);
    end;

  Consumer := Mock<IPubSubEventsDrivenConsumer>.Create;
  Consumer2 := Mock<IPubSubEventsDrivenConsumer>.Create;
  Broker := TMemoryPubSubEventsDrivenBroker<string>.Create;
  try
    Event1ExpectedPayload := MockUtils.SomeString;
    Event2ExpectedPayload := MockUtils.SomeString;

    Assert.WillNotRaiseAny(
      procedure
      begin
        Broker.Subscribe(Consumer, TEventsDrivenUtilities.FormatKey('Channel', 'Event1'), OnNotify1);
        Broker.Subscribe(Consumer2, TEventsDrivenUtilities.FormatKey('Channel', 'Event2'), OnNotify2);
        Broker.Subscribe(Consumer2, TEventsDrivenUtilities.FormatKey('Channel', 'Event3'), OnNotify3);
        Broker.Push(TEventsDrivenUtilities.FormatKey('Channel', 'Event1'), Event1ExpectedPayload);
        Broker.Push(TEventsDrivenUtilities.FormatKey('Channel', 'Event2'), Event2ExpectedPayload);
        Broker.Unsubscribe(Consumer2, TEventsDrivenUtilities.FormatKey('Channel', 'Event3'));
        Broker.Push(TEventsDrivenUtilities.FormatKey('Channel', 'Event2'), Event2ExpectedPayload);
        Broker.Push(TEventsDrivenUtilities.FormatKey('Channel', 'Event3'), Event2ExpectedPayload);
      end);

    Assert.AreEqual(Event1ExpectedPayload, Event1Payload);
    Assert.AreEqual(1, Event1Count);
    Assert.AreEqual(Event2ExpectedPayload, Event2Payload);
    Assert.AreEqual(2, Event2Count);
    Assert.AreEqual(0, Event3Count);
  finally
    Broker.Free;
  end;

  OnNotify1 := nil;
  OnNotify2 := nil;
  OnNotify3 := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TMemoryPubSubEventsDrivenBrokerTests);
end.
