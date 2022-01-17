unit Fido.EventsDriven.Listener.Queue.Test;

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
  Fido.EventsDriven.Consumer.Queue.Intf,
  Fido.EventsDriven.Listener.Queue;

type
  [TestFixture]
  TEventsDrivenQueueListenerTests = class
  public
    [Test]
    procedure StopDoesNotRaiseAnyException;

    [Test]
    procedure SubscribeToDoesNotRaiseAnyException;

    [Test]
    procedure UnsubscribeFromDoesNotRaiseAnyException;
  end;

implementation

procedure TEventsDrivenQueueListenerTests.StopDoesNotRaiseAnyException;
var
  Listener: IEventsDrivenListener;
  QueueConsumer: Mock<IEventsDrivenQueueConsumer>;
begin
  QueueConsumer := Mock<IEventsDrivenQueueConsumer>.Create;

  Listener := TEventsDrivenQueueListener.Create(
    function: IEventsDrivenQueueConsumer
    begin
      Result := QueueConsumer;
    end);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Listener.Stop;
    end);

  Listener := nil;
end;

procedure TEventsDrivenQueueListenerTests.SubscribeToDoesNotRaiseAnyException;
var
  Listener: IEventsDrivenListener;
  QueueConsumer: Mock<IEventsDrivenQueueConsumer>;
begin
  QueueConsumer := Mock<IEventsDrivenQueueConsumer>.Create;

  Listener := TEventsDrivenQueueListener.Create(
    function: IEventsDrivenQueueConsumer
    begin
      Result := QueueConsumer;
    end);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Listener.SubscribeTo(MockUtils.SomeString, MockUtils.SomeString, TConsumerData.Create(nil, MockUtils.SomeString));
    end);

  Listener := nil;
end;

procedure TEventsDrivenQueueListenerTests.UnsubscribeFromDoesNotRaiseAnyException;
var
  Listener: IEventsDrivenListener;
  QueueConsumer: Mock<IEventsDrivenQueueConsumer>;
begin
  QueueConsumer := Mock<IEventsDrivenQueueConsumer>.Create;

  Listener := TEventsDrivenQueueListener.Create(
    function: IEventsDrivenQueueConsumer
    begin
      Result := QueueConsumer;
    end);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Listener.UnsubscribeFrom(MockUtils.SomeString, MockUtils.SomeString);
    end);

  Listener := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TEventsDrivenQueueListenerTests);
end.
