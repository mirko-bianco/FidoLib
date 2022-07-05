unit Fido.EventsDriven.Publisher.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  DUnitX.TestFramework,

  Spring,
  Spring.Mocking,

  Fido.Functional,
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
  Publisher: IEventsDrivenPublisher<string>;
  Producer: Mock<IEventsDrivenProducer<string>>;
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

  Producer := Mock<IEventsDrivenProducer<string>>.Create;
  Producer.Setup.Returns<Context<Boolean>>(Context<Boolean>.New(True)).When.Push(Key, Payload);

  Publisher := TEventsDrivenPublisher<string>.Create(
    function: IEventsDrivenProducer<string>
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
  Producer.Received(Times.Once).Push(Key, Payload, INFINITE);
  Producer.Received(Times.Never).Push(Arg.IsNotIn<string>([Key]), Arg.IsNotIn<string>([Payload]), Arg.IsNotIn<Cardinal>([INFINITE]));
end;

initialization
  TDUnitX.RegisterTestFixture(TEventsDrivenPublisherTests);
end.
