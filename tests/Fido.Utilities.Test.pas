unit Fido.Utilities.Test;


interface

uses
  System.SysUtils,
  DUnitX.TestFramework,

  Fido.Utilities;

type
  [TestFixture]
  TUtilitiesTests = class
  public
    [Test]
    procedure GuardCheckNotNullAndSetReturnsIfValueIsNotNil;

    [Test]
    procedure GuardCheckNotNullAndSetThrowsIfValueIsNil;
  end;

  ITestValue = interface
  ['{1FC4989C-9756-4823-B299-CD8C1D90F0CE}']
  end;

  TestValue = class(TInterfacedObject, ITestValue)
  end;

implementation

{ TUtilitiesTests }

procedure TUtilitiesTests.GuardCheckNotNullAndSetReturnsIfValueIsNotNil;
var
  Input, Value: ITestValue;
begin
  Input := TestValue.Create;
  Value := Guard.CheckNotNullAndSet(Input, 'TestValue');
  Assert.AreEqual(Input, Value);
end;

procedure TUtilitiesTests.GuardCheckNotNullAndSetThrowsIfValueIsNil;
var
  Input: ITestValue;
begin
  Assert.WillRaise(procedure
    begin
      Guard.CheckNotNullAndSet(Input, 'TestValue');
    end,
  EArgumentNilException);
end;

initialization
  TDUnitX.RegisterTestFixture(TUtilitiesTests);
end.
