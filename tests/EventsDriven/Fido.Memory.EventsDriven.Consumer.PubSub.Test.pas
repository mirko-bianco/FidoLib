unit Fido.Memory.EventsDriven.Consumer.PubSub.Test;

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
  Fido.EventsDriven.Consumer.PubSub.Intf,
  Fido.EventsDriven.Broker.PubSub.Intf,
  Fido.Memory.EventsDriven.Consumer.PubSub;

type
  IPubSubEventsDrivenBroker = IPubSubEventsDrivenBroker<string>;
  IPubSubEventsDrivenConsumer = IPubSubEventsDrivenConsumer<string>;

  [TestFixture]
  TMemoryPubSubEventsDrivenConsumerTests = class
  public
    [Test]
    procedure SubscribeDoesNotRaiseAnyException;

    [Test]
    procedure UnsubscribeDoesNotRaiseAnyException;

    [Test]
    procedure StopDoesNotRaiseAnyException;
  end;

  TTestBroker = class(TInterfacedObject, IPubSubEventsDrivenBroker)
    function Push(const Key: string; const Payload: string; const Timeout: Cardinal = INFINITE): Context<Boolean>;

    procedure Subscribe(const Consumer: IPubSubEventsDrivenConsumer; const Key: string; const OnNotify: TProc<string, string>);
    procedure Unsubscribe(const Consumer: IPubSubEventsDrivenConsumer; const Key: string);

    procedure Stop(const Consumer: IPubSubEventsDrivenConsumer);
  end;

implementation

procedure TMemoryPubSubEventsDrivenConsumerTests.StopDoesNotRaiseAnyException;
begin
  Assert.WillNotRaiseAny(
    procedure
    var
      Consumer: IPubSubEventsDrivenConsumer<string>;
      Broker: IPubSubEventsDrivenBroker;
    begin
      Broker := TTestBroker.Create;

      Consumer := TMemoryPubSubEventsDrivenConsumer<string>.Create(Broker);
      Consumer.Stop;
    end);
end;

procedure TMemoryPubSubEventsDrivenConsumerTests.SubscribeDoesNotRaiseAnyException;
begin
  Assert.WillNotRaiseAny(
    procedure
    var
      Consumer: IPubSubEventsDrivenConsumer<string>;
      Broker: IPubSubEventsDrivenBroker;
    begin
      Broker := TTestBroker.Create;

      Consumer := TMemoryPubSubEventsDrivenConsumer<string>.Create(Broker);
      Consumer.Subscribe(
        MockUtils.SomeString,
        MockUtils.SomeString,
        procedure(Arg1: string; Arg2: string)
        begin
        end);
    end);
end;

procedure TMemoryPubSubEventsDrivenConsumerTests.UnsubscribeDoesNotRaiseAnyException;
begin
  Assert.WillNotRaiseAny(
    procedure
    var
      Consumer: IPubSubEventsDrivenConsumer<string>;
      Broker: IPubSubEventsDrivenBroker;
    begin
      Broker := TTestBroker.Create;

      Consumer := TMemoryPubSubEventsDrivenConsumer<string>.Create(Broker);
      Consumer.Unsubscribe(MockUtils.SomeString, MockUtils.SomeString);
    end);
end;

{ TTestBroker }

function TTestBroker.Push(const Key, Payload: string; const Timeout: Cardinal): Context<Boolean>;
begin
  Result := Context<Boolean>.New(True);
end;

procedure TTestBroker.Stop(const Consumer: IPubSubEventsDrivenConsumer);
begin

end;

procedure TTestBroker.Subscribe(const Consumer: IPubSubEventsDrivenConsumer; const Key: string; const OnNotify: TProc<string, string>);
begin

end;

procedure TTestBroker.Unsubscribe(const Consumer: IPubSubEventsDrivenConsumer; const Key: string);
begin

end;

initialization
  TDUnitX.RegisterTestFixture(TMemoryPubSubEventsDrivenConsumerTests);
end.
