unit Fido.EventsDriven.Listener.PubSub.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  DUnitX.TestFramework,

  Spring,
  Spring.Mocking,

  Fido.Testing.Mock.Utils,
  Fido.EventsDriven.Utils,
  Fido.EventsDriven.Listener.Intf,
  Fido.EventsDriven.Consumer.PubSub.Intf,
  Fido.EventsDriven.Listener.PubSub;

type
  [TestFixture]
  TEventsDrivenPubSubListenerTests = class
  public
    [Test]
    procedure StopDoesNotRaiseAnyException;

    [Test]
    procedure SubscribeToDoesNotRaiseAnyException;

    [Test]
    procedure UnsubscribeFromDoesNotRaiseAnyException;
  end;

implementation

procedure TEventsDrivenPubSubListenerTests.StopDoesNotRaiseAnyException;
var
  Listener: IEventsDrivenListener;
  PubSubConsumer: Mock<IEventsDrivenPubSubConsumer>;
begin
  PubSubConsumer := Mock<IEventsDrivenPubSubConsumer>.Create;

  Listener := TEventsDrivenPubSubListener.Create(PubSubConsumer);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Listener.Stop;
    end);
end;

procedure TEventsDrivenPubSubListenerTests.SubscribeToDoesNotRaiseAnyException;
var
  Listener: IEventsDrivenListener;
  PubSubConsumer: Mock<IEventsDrivenPubSubConsumer>;
begin
  PubSubConsumer := Mock<IEventsDrivenPubSubConsumer>.Create;

  Listener := TEventsDrivenPubSubListener.Create(PubSubConsumer);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Listener.SubscribeTo(MockUtils.SomeString, MockUtils.SomeString, TConsumerData.Create(nil, MockUtils.SomeString));
    end);
end;

procedure TEventsDrivenPubSubListenerTests.UnsubscribeFromDoesNotRaiseAnyException;
var
  Listener: IEventsDrivenListener;
  PubSubConsumer: Mock<IEventsDrivenPubSubConsumer>;
begin
  PubSubConsumer := Mock<IEventsDrivenPubSubConsumer>.Create;

  Listener := TEventsDrivenPubSubListener.Create(PubSubConsumer);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Listener.UnsubscribeFrom(MockUtils.SomeString, MockUtils.SomeString);
    end);
end;

initialization
  TDUnitX.RegisterTestFixture(TEventsDrivenPubSubListenerTests);
end.
