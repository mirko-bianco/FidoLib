unit Fido.OwningObject.Test;

interface

uses
  DUnitX.TestFramework,
  SysUtils,

  Fido.OwningObject;

type
  TChildObject = class
  strict private class var
    FFreedCount: Integer;
  public
    class function FreedCount: Integer;
    class procedure ResetFreedCount;
  public
    destructor Destroy; override;
  end;

  TTestObject = class(TOwningObject)
  strict private
    FChildObject: TChildObject;
  public
    constructor Create;

    property ChildObject: TChildObject read FChildObject;
  end;

  TTestObjectArray = class(TOwningObject)
  strict private
    FChildObjectArray: TArray<TChildObject>;
  public
    constructor Create;

    property ChildObjectArray: TArray<TChildObject> read FChildObjectArray;
  end;

  [TestFixture]
  TOwningObjectTests = class
  public
    [Test]
    procedure OwningObjectFreesItsChildObject;

    [Test]
    procedure OwningObjectFreesItsChildrenObjects;
  end;

implementation

procedure TOwningObjectTests.OwningObjectFreesItsChildObject;
var
  TestObject: TTestObject;
begin
  TChildObject.ResetFreedCount;

  TestObject := TTestObject.Create;
  TestObject.Free;

  Assert.AreEqual(1, TChildObject.FreedCount);
end;

procedure TOwningObjectTests.OwningObjectFreesItsChildrenObjects;
var
  TestObjectArray: TTestObjectArray;
begin
  TChildObject.ResetFreedCount;

  TestObjectArray := TTestObjectArray.Create;
  TestObjectArray.Free;

  Assert.AreEqual(3, TChildObject.FreedCount);
end;

{ TChildObject }

destructor TChildObject.Destroy;
begin
  Inc(FFreedCount);
  inherited;
end;

class function TChildObject.FreedCount: Integer;
begin
  Result := FFreedCount;
end;

class procedure TChildObject.ResetFreedCount;
begin
  FFreedCount := 0;
end;

{ TTestObject }

constructor TTestObject.Create;
begin
  inherited;
  FChildObject := TChildObject.Create;
end;

{ TTestObjectArray }

constructor TTestObjectArray.Create;
begin
  inherited;
  SetLength(FChildObjectArray, 3);
  FChildObjectArray[0] := TChildObject.Create;
  FChildObjectArray[1] := TChildObject.Create;
  FChildObjectArray[2] := TChildObject.Create;
end;

initialization
  TDUnitX.RegisterTestFixture(TOwningObjectTests);
end.
