unit Fido.EventsDriven.Utils.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Rtti,
  System.Threading,
  DUnitX.TestFramework,

  Spring,
  Spring.Collections,
  Spring.Mocking,

  Fido.JSON.Marshalling,
  Fido.Testing.Mock.Utils,
  Fido.EventsDriven.Utils;

type
  [TestFixture]
  TEventsDrivenUtilsTests = class
  public
    [Test]
    procedure FormatKeyWorksAsExpected;

    [Test]
    procedure PayloadToMethodParamTranslateCorrectlyWhenPayloadIsAsSingleValue;

    [Test]
    procedure StringPayloadToMethodParamsTranslatesCorrectlyWhenPayloadIsASingleValueAsAnArray;

    [Test]
    procedure StringPayloadToMethodParamsTranslatesCorrectlyWhenPayloadAreMultipleValuesAsAnArray;

    [Test]
    procedure ArrayOfTValuePayloadToMethodParamsTranslatesCorrectlyWithOneValue;

    [Test]
    procedure ArrayOfTValuePayloadToMethodParamsTranslatesCorrectlyWithMultipleValues;

    [Test]
    procedure PayloadToMethodParamsFailesWithNonSupportedType;
  end;

  {$M+}
  TTestData = class
  private
    FValue: string;
  published
    function Value: string;
    procedure SetValue(const Value: string);
  end;

  TTestObject = class
    procedure TestMethod1(const StringParam: string);

    procedure TestMethod2(const StringParam: string; const BooleanParam: Boolean; const TestData: TTestData);

    procedure TestMethod3(const BooleanParam: Boolean);
  end;
  {$M-}

implementation

procedure TEventsDrivenUtilsTests.FormatKeyWorksAsExpected;
var
  ExpectedResult: string;
  Channel: string;
  EventName: string;
begin
  Channel := MockUtils.SomeString;
  EventName := MockUtils.SomeString;
  ExpectedResult := Format('%s::%s', [Channel, EventName]);
  Assert.AreEqual(ExpectedResult, TEventsDrivenUtilities.FormatKey(Channel, EventName));
end;

procedure TEventsDrivenUtilsTests.PayloadToMethodParamsFailesWithNonSupportedType;
var
  Ctx: TRttiContext;
begin
  Ctx := TRttiContext.Create;

  Assert.WillRaise(
    procedure
    begin
      TCollections.CreateList<TRttiMethod>(Ctx.GetType(TTestObject).GetMethods).Where(
        function(const Method: TRttiMethod): Boolean
        begin
          Result := Method.Name = 'TestMethod3';
        end).ForEach(
        procedure(const Method: TRttiMethod)
        begin
          TEventsDrivenUtilities.PayloadToMethodParams(MockUtils.SomeBoolean, Method);
        end);
    end,
    EEventsDrivenUtilities,
    'TEventsDrivenUtilities.PayloadToMethodParams: Type System.Boolean not supported.');
end;

procedure TEventsDrivenUtilsTests.PayloadToMethodParamTranslateCorrectlyWhenPayloadIsAsSingleValue;
var
  StringValue: string;
  Ctx: TRttiContext;
  Payload: string;
  Values: TArray<TValue>;
begin
  StringValue := MockUtils.SomeString;
  Payload := JSONMarshaller.From<string>(StringValue);

  Ctx := TRttiContext.Create;
  TCollections.CreateList<TRttiMethod>(Ctx.GetType(TTestObject).GetMethods).Where(
    function(const Method: TRttiMethod): Boolean
    begin
      Result := Method.Name = 'TestMethod1';
    end).ForEach(
    procedure(const Method: TRttiMethod)
    begin
      Values := TEventsDrivenUtilities.PayloadToMethodParams(Payload, Method);
    end);

  Assert.AreEqual(1, Length(Values));
  Assert.AreEqual(StringValue, Values[0].AsType<string>)
end;

procedure TEventsDrivenUtilsTests.ArrayOfTValuePayloadToMethodParamsTranslatesCorrectlyWithMultipleValues;
var
  TestObject: Shared<TTestObject>;
  Ctx: TRttiContext;
  Result: TArray<TValue>;
  StringValue: string;
  BooleanValue: Boolean;
  TestDataValue: Shared<TTestData>;
begin
  StringValue := MockUtils.SomeString;
  BooleanValue := MockUtils.SomeBoolean;
  TestDataValue := TTestData.Create;
  TestDataValue.Value.SetValue(MockUtils.SomeString);

  Ctx := TRttiContext.Create;

  TestObject := TTestObject.Create;

  TCollections.CreateList<TRttiMethod>(Ctx.GetType(TTestObject).GetMethods).Where(
    function(const Method: TRttiMethod): Boolean
    begin
      Result := Method.Name = 'TestMethod2'
    end).ForEach(
    procedure(const Method: TRttiMethod)
    begin
      Result := TEventsDrivenUtilities.PayloadToMethodParams<TArray<TValue>>([StringValue, BooleanValue, TestDataValue.Value], Method);
    end);

  Assert.AreEqual(3, Length(Result));
  Assert.AreEqual(StringValue, Result[0].AsType<string>);
  Assert.AreEqual(BooleanValue, Result[1].AsType<Boolean>);
  Assert.AreEqual<TTestData>(TestDataValue, Result[2].AsType<TTestData>);
end;

procedure TEventsDrivenUtilsTests.ArrayOfTValuePayloadToMethodParamsTranslatesCorrectlyWithOneValue;
var
  TestObject: Shared<TTestObject>;
  Ctx: TRttiContext;
  Result: TArray<TValue>;
  Message: string;
begin
  Message := MockUtils.SomeString;
  Ctx := TRttiContext.Create;

  TestObject := TTestObject.Create;

  TCollections.CreateList<TRttiMethod>(Ctx.GetType(TTestObject).GetMethods).Where(
    function(const Method: TRttiMethod): Boolean
    begin
      Result := Method.Name = 'TestMethod1'
    end).ForEach(
    procedure(const Method: TRttiMethod)
    begin
      Result := TEventsDrivenUtilities.PayloadToMethodParams<TArray<TValue>>([Message], Method);
    end);

  Assert.AreEqual(1, Length(Result));
  Assert.AreEqual(Message, Result[0].AsType<string>);
end;

procedure TEventsDrivenUtilsTests.StringPayloadToMethodParamsTranslatesCorrectlyWhenPayloadAreMultipleValuesAsAnArray;
var
  StringValue: string;
  BooleanValue: Boolean;
  TestDataValue: Shared<TTestData>;
  Ctx: TRttiContext;
  Payload: string;
  Values: TArray<TValue>;
begin
  StringValue := MockUtils.SomeString;
  BooleanValue := MockUtils.SomeBoolean;
  TestDataValue := TTestData.Create;
  TestDataValue.Value.SetValue(MockUtils.SomeString);

  Payload := Format(
    '[%s, %s, %s]',
    [
     JSONMarshaller.From<string>(StringValue),
     JSONMarshaller.From<Boolean>(BooleanValue),
     JSONMarshaller.From<TTestData>(TestDataValue)
    ]);

  Ctx := TRttiContext.Create;
  TCollections.CreateList<TRttiMethod>(Ctx.GetType(TTestObject).GetMethods).Where(
    function(const Method: TRttiMethod): Boolean
    begin
      Result := Method.Name = 'TestMethod2';
    end).ForEach(
    procedure(const Method: TRttiMethod)
    begin
      Values := TEventsDrivenUtilities.PayloadToMethodParams(Payload, Method);
    end);

  Assert.AreEqual(3, Length(Values));
  Assert.AreEqual(StringValue, Values[0].AsType<string>);
  Assert.AreEqual(BooleanValue, Values[1].AsType<Boolean>);
  Assert.AreEqual(TestDataValue.Value.Value, Values[2].AsType<TTestData>.Value);
  Values[2].Free;
end;

procedure TEventsDrivenUtilsTests.StringPayloadToMethodParamsTranslatesCorrectlyWhenPayloadIsASingleValueAsAnArray;
var
  StringValue: string;
  Ctx: TRttiContext;
  Payload: string;
  Values: TArray<TValue>;
begin
  StringValue := MockUtils.SomeString;
  Payload := Format('[%s]', [JSONMarshaller.From<string>(StringValue)]);

  Ctx := TRttiContext.Create;
  TCollections.CreateList<TRttiMethod>(Ctx.GetType(TTestObject).GetMethods).Where(
    function(const Method: TRttiMethod): Boolean
    begin
      Result := Method.Name = 'TestMethod1';
    end).ForEach(
    procedure(const Method: TRttiMethod)
    begin
      Values := TEventsDrivenUtilities.PayloadToMethodParams(Payload, Method);
    end);

  Assert.AreEqual(1, Length(Values));
  Assert.AreEqual(StringValue, Values[0].AsType<string>)
end;

{ TTestObject }

procedure TTestObject.TestMethod1(const StringParam: string);
begin

end;

procedure TTestObject.TestMethod2(const StringParam: string; const BooleanParam: Boolean; const TestData: TTestData);
begin

end;

procedure TTestObject.TestMethod3(const BooleanParam: Boolean);
begin

end;

{ TestData }

procedure TTestData.SetValue(const Value: string);
begin
  FValue := Value;
end;

function TTestData.Value: string;
begin
  Result := FValue;
end;

initialization
  TDUnitX.RegisterTestFixture(TEventsDrivenUtilsTests);
end.
