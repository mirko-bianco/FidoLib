unit Fido.Immutable.Test;

{$I Jedi.inc}

interface

uses
  DUnitX.TestFramework,
  SysUtils,

  Fido.Immutable;

{$IFDEF DELPHIX_SYDNEY_UP}
type
  [TestFixture]
  TImmutableTests = class
  public
    [Test]
    procedure ImmutableCanBeAssigned;

    [Test]
    procedure ImmutableCanBeMade;

    [Test]
    procedure ImmutableCannotBeReassigned;

    [Test]
    procedure NonImmutableCanBeAssigned;
  end;
{$ENDIF}

implementation

{$IFDEF DELPHIX_SYDNEY_UP}
procedure TImmutableTests.ImmutableCanBeMade;
var
  ImmInteger: TImmutable<Integer>;
begin
  ImmInteger := TImmutable<Integer>.Make(100);

  Assert.AreEqual<Integer>(100, ImmInteger);
end;

procedure TImmutableTests.ImmutableCanBeAssigned;
var
  ImmInteger: TImmutable<Integer>;
begin
  ImmInteger := 100;

  Assert.AreEqual<Integer>(100, ImmInteger);
end;

procedure TImmutableTests.ImmutableCannotBeReassigned;
var
  ImmInteger: TImmutable<Integer>;
begin
  ImmInteger := 100;

  Assert.WillRaise(
    procedure
    begin
      ImmInteger := 50;
    end,
    EImmutableException);
end;

procedure TImmutableTests.NonImmutableCanBeAssigned;
var
  ImmInteger: TImmutable<Integer>;
  NonImmInteger: Integer;
begin
  ImmInteger := 100;

  Assert.WillNotRaiseAny(
    procedure
    begin
      NonImmInteger := ImmInteger;
    end);

  Assert.AreEqual(100, NonImmInteger);
end;

initialization
  TDUnitX.RegisterTestFixture(TImmutableTests);
{$ENDIF}
end.
