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
  TQueueEventsDrivenListenerTests = class
  public
    [Test]
    procedure StopDoesNotRaiseAnyException;

    [Test]
    procedure SubscribeToDoesNotRaiseAnyException;

    [Test]
    procedure UnsubscribeFromDoesNotRaiseAnyException;
  end;

implementation

procedure TQueueEventsDrivenListenerTests.StopDoesNotRaiseAnyException;
var
  Listener: IEventsDrivenListener;
  QueueConsumer: Mock<IQueueEventsDrivenConsumer>;
begin
  QueueConsumer := Mock<IQueueEventsDrivenConsumer>.Create;

  Listener := TQueueEventsDrivenListener.Create(
    function: IQueueEventsDrivenConsumer
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

procedure TQueueEventsDrivenListenerTests.SubscribeToDoesNotRaiseAnyException;
var
  Listener: IEventsDrivenListener;
  QueueConsumer: Mock<IQueueEventsDrivenConsumer>;
begin
  QueueConsumer := Mock<IQueueEventsDrivenConsumer>.Create;

  Listener := TQueueEventsDrivenListener.Create(
    function: IQueueEventsDrivenConsumer
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

procedure TQueueEventsDrivenListenerTests.UnsubscribeFromDoesNotRaiseAnyException;
var
  Listener: IEventsDrivenListener;
  QueueConsumer: Mock<IQueueEventsDrivenConsumer>;
begin
  QueueConsumer := Mock<IQueueEventsDrivenConsumer>.Create;

  Listener := TQueueEventsDrivenListener.Create(
    function: IQueueEventsDrivenConsumer
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
  TDUnitX.RegisterTestFixture(TQueueEventsDrivenListenerTests);
end.
