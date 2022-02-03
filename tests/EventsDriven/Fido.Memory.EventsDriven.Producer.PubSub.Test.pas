unit Fido.Memory.EventsDriven.Producer.PubSub.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  DUnitX.TestFramework,

  Spring,
  Spring.Mocking,

  Fido.Testing.Mock.Utils,
  Fido.EventsDriven.Producer.Intf,
  Fido.EventsDriven.Broker.PubSub.Intf,
  Fido.Memory.EventsDriven.Producer.PubSub;

type
  IPubSubEventsDrivenBroker = IPubSubEventsDrivenBroker<string>;

  [TestFixture]
  TMemoryPubSubEventsDrivenProducerTests = class
  public
    [Test]
    procedure PushDoesNotRaiseAnyException;
  end;

implementation

procedure TMemoryPubSubEventsDrivenProducerTests.PushDoesNotRaiseAnyException;
var
  Producer: IEventsDrivenProducer<string>;
  Broker: Mock<IPubSubEventsDrivenBroker>;
begin
  Broker := Mock<IPubSubEventsDrivenBroker>.Create;

  Producer := TMemoryPubSubEventsDrivenProducer<string>.Create(Broker);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Producer.Push(MockUtils.SomeString, MockUtils.SomeString);
    end);
end;

initialization
  TDUnitX.RegisterTestFixture(TMemoryPubSubEventsDrivenProducerTests);
end.
