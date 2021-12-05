unit Fido.Slots.Binding.Tests;

interface

uses
  System.Classes,
  System.SysUtils,
  DUnitX.TestFramework,

  Spring,

  Fido.DesignPatterns.Observable.Intf,
  Fido.DesignPatterns.Observable.Delegated,
  Fido.Slots.Intf,
  Fido.Slots,
  Fido.Slots.Attributes,
  Fido.Slots.Binding;

const
  TEST_NOTIFICATION = 'Test';

type
  ITestSignalActor = interface(IObservable)
    ['{C27D162E-C398-4C43-9943-44EA9F7F1193}']

    procedure Trigger;
  end;

  TTestSignalActor = class(TDelegatedOBservable, ITestSignalActor)
    procedure Trigger;
  end;

  TTestSlotActorWorking1 = class
  private
    FStatus: Boolean;
    FSignalActor: ITestSignalActor;
  public
    constructor Create(const SignalActor: ITestSignalActor);

    [SignalToSlot('FSignalActor', TEST_NOTIFICATION, stNotSynched)]
    procedure OnTrigger(const Flag: Boolean);

    function Status: Boolean;
  end;

  TTestSlotActorWorking2 = class
  private
    FStatus: Boolean;
    FSignalActor: ITestSignalActor;

  public
    constructor Create(const SignalActor: ITestSignalActor);

    function GetSignalActor: ITestSignalActor;

    [SignalToSlot('GetSignalActor', TEST_NOTIFICATION, stNotSynched)]
    procedure OnTrigger(const Flag: Boolean);

    function Status: Boolean;
  end;

  TTestSlotActorNotWorking1 = class
  private
    FSignalActor: ITestSignalActor;
  public
    constructor Create(const SignalActor: ITestSignalActor);

    [SignalToSlot('FSignalActorNotFound', TEST_NOTIFICATION, stNotSynched)]
    procedure OnTrigger(const Flag: Boolean);

  end;

  TTestSlotActorNotWorking2 = class
  private
    FSignalActor: ITestSignalActor;
  public
    constructor Create(const SignalActor: ITestSignalActor);

    function GetSignalActor: ITestSignalActor;

    [SignalToSlot('GetSignalActorNotFound', TEST_NOTIFICATION, stNotSynched)]
    procedure OnTrigger(const Flag: Boolean);

  end;

  TTestSlotActorNotWorking3 = class
  public
    function GetSignalActor(const Param: Boolean): ITestSignalActor;

    [SignalToSlot('GetSignalActor', TEST_NOTIFICATION, stNotSynched)]
    procedure OnTrigger(const Flag: Boolean);
  end;

  TTestSlotActorNotWorking4 = class
  public
    procedure GetSignalActor;

    [SignalToSlot('GetSignalActor', TEST_NOTIFICATION, stNotSynched)]
    procedure OnTrigger(const Flag: Boolean);
  end;

  TTestSlotActorNotWorking5 = class
  public
    function GetSignalActor: Integer;

    [SignalToSlot('GetSignalActor', TEST_NOTIFICATION, stNotSynched)]
    procedure OnTrigger(const Flag: Boolean);
  end;

  TTestSlotActorNotWorking6 = class
  private
    FSignalActor: Integer;
  public
    constructor Create;

    [SignalToSlot('FSignalActor', TEST_NOTIFICATION, stNotSynched)]
    procedure OnTrigger(const Flag: Boolean);
  end;

  [TestFixture]
  TSlotsBindingTests = class
  public
    [Test]
    procedure SlotsBindingWorksWhenFieldIsFoundParametersAreCorrect;

    [Test]
    procedure SlotsBindingWorksWhenMethodIsFoundParametersAreCorrect;

    [Test]
    procedure SlotBindingRaisesESlotBindingWhenFieldIsFoundAndSlotActorIsWrong;

    [Test]
    procedure SlotBindingRaisesESlotBindingWhenMethodIsFoundAndSlotActorIsWrong;

    [Test]
    procedure SlotBindingRaisesESlotBindingWhenMethodIsFoundButIsNotParameterless;

    [Test]
    procedure SlotBindingRaisesESlotBindingWhenMethodIsFoundButIsAProcedure;

    [Test]
    procedure SlotBindingRaisesESlotBindingWhenMethodIsFoundButIsNotTheCorrectType;

    [Test]
    procedure SlotBindingRaisesESlotBindingWhenFieldIsFoundButIsNotTheCorrectType;
  end;

implementation

procedure TSlotsBindingTests.SlotsBindingWorksWhenFieldIsFoundParametersAreCorrect;
var
  SignalActor: ITestSignalActor;
  SlotActor: Shared<TTestSlotActorWorking1>;
  TheSlots: ISlots;
begin
  SignalActor := TTestSignalActor.Create(nil);
  SlotActor := TTestSlotActorWorking1.Create(SignalActor);
  TheSlots := TSLots.Create;
  Assert.WillNotRaiseAny(
    procedure
    begin
      Slots.Register(TheSlots, SignalActor, SlotActor.Value);
      SignalActor.Trigger;
    end);
  Assert.AreEqual(True, SlotActor.Value.Status);
end;

procedure TSlotsBindingTests.SlotsBindingWorksWhenMethodIsFoundParametersAreCorrect;
var
  SignalActor: ITestSignalActor;
  SlotActor: Shared<TTestSlotActorWorking2>;
  TheSlots: ISlots;
begin
  SignalActor := TTestSignalActor.Create(nil);
  SlotActor := TTestSlotActorWorking2.Create(SignalActor);
  TheSlots := TSLots.Create;
  Assert.WillNotRaiseAny(
    procedure
    begin
      Slots.Register(TheSlots, SignalActor, SlotActor.Value);
      SignalActor.Trigger;
    end);
  Assert.AreEqual(True, SlotActor.Value.Status);
end;

procedure TSlotsBindingTests.SlotBindingRaisesESlotBindingWhenFieldIsFoundButIsNotTheCorrectType;
var
  SignalActor: ITestSignalActor;
  SlotActor: Shared<TTestSlotActorNotWorking6>;
  TheSlots: ISlots;
begin
  SignalActor := TTestSignalActor.Create(nil);
  SlotActor := TTestSlotActorNotWorking6.Create;
  TheSlots := TSLots.Create;
  Assert.WillRaise(
    procedure
    begin
      Slots.Register(TheSlots, SignalActor, SlotActor.Value);
    end,
    ESlotBinding);
end;

procedure TSlotsBindingTests.SlotBindingRaisesESlotBindingWhenMethodIsFoundAndSlotActorIsWrong;
var
  SignalActor: ITestSignalActor;
  SlotActor: Shared<TTestSlotActorNotWorking2>;
  TheSlots: ISlots;
begin
  SignalActor := TTestSignalActor.Create(nil);
  SlotActor := TTestSlotActorNotWorking2.Create(SignalActor);
  TheSlots := TSLots.Create;
  Assert.WillRaise(
    procedure
    begin
      Slots.Register(TheSlots, SignalActor, SlotActor.Value);
    end,
    ESlotBinding);
end;

procedure TSlotsBindingTests.SlotBindingRaisesESlotBindingWhenMethodIsFoundButIsAProcedure;
var
  SignalActor: ITestSignalActor;
  SlotActor: Shared<TTestSlotActorNotWorking4>;
  TheSlots: ISlots;
begin
  SignalActor := TTestSignalActor.Create(nil);
  SlotActor := TTestSlotActorNotWorking4.Create;
  TheSlots := TSLots.Create;
  Assert.WillRaise(
    procedure
    begin
      Slots.Register(TheSlots, SignalActor, SlotActor.Value);
    end,
    ESlotBinding);
end;

procedure TSlotsBindingTests.SlotBindingRaisesESlotBindingWhenMethodIsFoundButIsNotParameterless;
var
  SignalActor: ITestSignalActor;
  SlotActor: Shared<TTestSlotActorNotWorking3>;
  TheSlots: ISlots;
begin
  SignalActor := TTestSignalActor.Create(nil);
  SlotActor := TTestSlotActorNotWorking3.Create;
  TheSlots := TSLots.Create;
  Assert.WillRaise(
    procedure
    begin
      Slots.Register(TheSlots, SignalActor, SlotActor.Value);
    end,
    ESlotBinding);
end;

procedure TSlotsBindingTests.SlotBindingRaisesESlotBindingWhenMethodIsFoundButIsNotTheCorrectType;
var
  SignalActor: ITestSignalActor;
  SlotActor: Shared<TTestSlotActorNotWorking5>;
  TheSlots: ISlots;
begin
  SignalActor := TTestSignalActor.Create(nil);
  SlotActor := TTestSlotActorNotWorking5.Create;
  TheSlots := TSLots.Create;
  Assert.WillRaise(
    procedure
    begin
      Slots.Register(TheSlots, SignalActor, SlotActor.Value);
    end,
    ESlotBinding);
end;

procedure TSlotsBindingTests.SlotBindingRaisesESlotBindingWhenFieldIsFoundAndSlotActorIsWrong;
var
  SignalActor: ITestSignalActor;
  SlotActor: Shared<TTestSlotActorNotWorking1>;
  TheSlots: ISlots;
begin
  SignalActor := TTestSignalActor.Create(nil);
  SlotActor := TTestSlotActorNotWorking1.Create(SignalActor);
  TheSlots := TSLots.Create;
  Assert.WillRaise(
    procedure
    begin
      Slots.Register(TheSlots, SignalActor, SlotActor.Value);
    end,
    ESlotBinding);
end;

{ TTestSignalActor }

procedure TTestSignalActor.Trigger;
begin
  Broadcast(TEST_NOTIFICATION, TValue.From<TArray<TValue>>([True]));
end;

{ TTestSlotActor }

constructor TTestSlotActorWorking1.Create(const SignalActor: ITestSignalActor);
begin
  inherited Create;
  FSignalActor := SignalActor;
  FStatus := False;
end;

procedure TTestSlotActorWorking1.OnTrigger(const Flag: Boolean);
begin
  FStatus := Flag;
end;

function TTestSlotActorWorking1.Status: Boolean;
begin
  Result := FStatus;
end;

{ TTestSlotActorNotWorking1 }

constructor TTestSlotActorNotWorking1.Create(const SignalActor: ITestSignalActor);
begin
  inherited Create;
  FSignalActor := SignalActor;
end;

procedure TTestSlotActorNotWorking1.OnTrigger(const Flag: Boolean);
begin
end;

{ TTestSlotActorNotWorking2 }

constructor TTestSlotActorNotWorking2.Create(const SignalActor: ITestSignalActor);
begin
  inherited Create;
  FSignalActor := SignalActor;
end;

function TTestSlotActorNotWorking2.GetSignalActor: ITestSignalActor;
begin
  Result := FSignalActor;
end;

procedure TTestSlotActorNotWorking2.OnTrigger(const Flag: Boolean);
begin

end;

{ TTestSlotActorWorking2 }

constructor TTestSlotActorWorking2.Create(const SignalActor: ITestSignalActor);
begin
  inherited Create;
  FSignalActor := SignalActor;
  FStatus := False;
end;

function TTestSlotActorWorking2.GetSignalActor: ITestSignalActor;
begin
  Result := FSignalActor;
end;

procedure TTestSlotActorWorking2.OnTrigger(const Flag: Boolean);
begin
  FStatus := Flag;
end;

function TTestSlotActorWorking2.Status: Boolean;
begin
  Result := FStatus;
end;

{ TTestSlotActorNotWorking3 }

function TTestSlotActorNotWorking3.GetSignalActor(const Param: Boolean): ITestSignalActor;
begin

end;

procedure TTestSlotActorNotWorking3.OnTrigger(const Flag: Boolean);
begin

end;

{ TTestSlotActorNotWorking4 }

procedure TTestSlotActorNotWorking4.GetSignalActor;
begin

end;

procedure TTestSlotActorNotWorking4.OnTrigger(const Flag: Boolean);
begin

end;

{ TTestSlotActorNotWorking5 }

function TTestSlotActorNotWorking5.GetSignalActor: Integer;
begin
  Result := 0
end;

procedure TTestSlotActorNotWorking5.OnTrigger(const Flag: Boolean);
begin

end;

{ TTestSlotActorNotWorking6 }

constructor TTestSlotActorNotWorking6.Create;
begin
  FSignalActor := 0;
end;

procedure TTestSlotActorNotWorking6.OnTrigger(const Flag: Boolean);
begin

end;

initialization
  TDUnitX.RegisterTestFixture(TSlotsBindingTests);
end.
