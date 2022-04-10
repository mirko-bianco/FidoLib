unit Fido.Utilities.Test;


interface

uses
  System.SysUtils,
  DUnitX.TestFramework,

  Fido.Testing.Mock.Utils,
  Fido.Utilities;

type
  [TestFixture]
  TUtilitiesTests = class
  public
    [Test]
    procedure GuardCheckNotNullAndSetReturnsIfValueIsNotNil;

    [Test]
    procedure GuardCheckNotNullAndSetThrowsIfValueIsNil;

    [Test]
    procedure GuardCheckNotNullAndSetReturnsIfAnonymousIsNotNil;

    [Test]
    procedure GuardCheckNotNullAndSetThrowsIfAnonymousIsNil;

    [Test]
    procedure CheckAndSetReturnsIfPredicateReturnsTrue;
    [Test]
    procedure CheckAndSetThrowsIfPredicateReturnsFalse;
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
  Value := Utilities.CheckNotNullAndSet(Input, 'TestValue');
  Assert.AreEqual(Input, Value);
end;

procedure TUtilitiesTests.GuardCheckNotNullAndSetThrowsIfValueIsNil;
var
  Input: ITestValue;
begin
  Assert.WillRaise(procedure
    begin
      Utilities.CheckNotNullAndSet(Input, 'TestValue');
    end,
  EArgumentNilException);
end;

procedure TUtilitiesTests.CheckAndSetReturnsIfPredicateReturnsTrue;
var
  Source: string;
  Destination: string;
begin
  Source := MockUtils.SomeString;

  Destination := Utilities.CheckAndSet<string>(Source, Utilities.F.IsNotEmpty(Source), 'there is an error');

  Assert.AreEqual(Source, Destination);
end;

procedure TUtilitiesTests.CheckAndSetThrowsIfPredicateReturnsFalse;
begin
  Assert.WillRaise(procedure
    begin
      Utilities.CheckAndSet<string>('', Utilities.F.IsNotEmpty(''), 'there is an error');
    end,
  EArgumentNilException);
end;

procedure TUtilitiesTests.GuardCheckNotNullAndSetReturnsIfAnonymousIsNotNil;
var
  LCounter: Integer;
  LProc, LProcOriginal: TProc;
  LFunc, LFuncOriginal: TFunc<Integer>;
begin
  LProcOriginal := procedure
    begin
      Inc(LCounter);
    end;

  LFuncOriginal := function: Integer
    begin
      Result := 42;
    end;

  LProc := Utilities.CheckNotNullAndSet<TProc>(LProcOriginal, 'Proc expected');
  LFunc := Utilities.CheckNotNullAndSet<TFunc<Integer>>(LFuncOriginal, 'Proc expected');

  LProc;
  LProcOriginal;
  Assert.AreEqual(2, LCounter);

  Assert.AreEqual(LFuncOriginal, LFunc);
end;

procedure TUtilitiesTests.GuardCheckNotNullAndSetThrowsIfAnonymousIsNil;
var
  LProc: TProc;
  LFunc: TFunc<Integer>;
begin
  Assert.WillRaise(procedure
    begin
      Utilities.CheckNotNullAndSet<TProc>(LProc, 'TestValue');
    end,
  EArgumentNilException);

  Assert.WillRaise(procedure
    begin
      Utilities.CheckNotNullAndSet<TFunc<Integer>>(LFunc, 'TestValue');
    end,
  EArgumentNilException);
end;

initialization
  TDUnitX.RegisterTestFixture(TUtilitiesTests);
end.
