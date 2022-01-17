unit Fido.EventsDriven.Publisher.Test;

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
  Fido.EventsDriven.Publisher.Intf,
  Fido.EventsDriven.Producer.Intf,
  Fido.EventsDriven.Publisher;

type
  [TestFixture]
  TEventsDrivenPublisherTests = class
  public
    [Test]
    procedure TriggerDoesNotRaiseAnyException;
  end;

implementation

procedure TEventsDrivenPublisherTests.TriggerDoesNotRaiseAnyException;
var
  Publisher: IEventsDrivenPublisher;
  Producer: Mock<IEventsDrivenProducer>;
  Result: Boolean;
  Channel: string;
  EventName: string;
  Payload: string;
  Key: string;
begin
  Channel := MockUtils.SomeString;
  EventName := MockUtils.SomeString;
  Payload := MockUtils.SomeString;
  Key := TEventsDrivenUtilities.FormatKey(Channel, EventName);

  Producer := Mock<IEventsDrivenProducer>.Create;
  Producer.Setup.Returns<Boolean>(True).When.Push(Key, Payload);

  Publisher := TEventsDrivenPublisher.Create(
    function: IEventsDrivenProducer
    begin
      Result := Producer;
    end);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result := Publisher.Trigger(Channel, EventName, Payload);
    end);

  Publisher := nil;

  Assert.AreEqual(True, Result);
  Producer.Received(Times.Once).Push(Key, Payload);
  Producer.Received(Times.Never).Push(Arg.IsNotIn<string>([Key]), Arg.IsNotIn<string>([Payload]));
end;

initialization
  TDUnitX.RegisterTestFixture(TEventsDrivenPublisherTests);
end.
