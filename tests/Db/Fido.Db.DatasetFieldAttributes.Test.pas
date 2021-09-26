unit Fido.Db.DatasetFieldAttributes.Test;

interface
uses
  DUnitX.TestFramework,

  Spring,

  Fido.Testing.Mock.Utils,

  Fido.Types,
  Fido.Db.DatasetFieldAttributes.Intf,
  Fido.Db.DatasetFieldAttributes;

type

  [TestFixture]
  TFieldViewAttributesTest = class(TObject)
  public
    [Test]
    procedure SetFieldViewAttributeAddsANewAttributeWhenFieldNameIsNotYetPresent;
    [Test]
    procedure SetFieldViewAttributeUpdatesSameAttributeWhenFieldNameIsAlreadyPresent;
    [Test]
    procedure GetFieldNamesEnumeratorReturnsAnOrderedEnumerator;
  end;

implementation


{ TFieldViewAttributesTest }

procedure TFieldViewAttributesTest.GetFieldNamesEnumeratorReturnsAnOrderedEnumerator;
var
  Attributes: IDatasetFieldAttributes;

  FieldName1: string;
  FieldName2: string;
  FieldName3: string;
  FieldName4: string;

  SomeLength: Integer;
  SomeLabel: string;
  SomeReadOnly: Boolean;
  SomeMask: string;
  SomeVisible: Boolean;
begin
  Attributes := TDatasetFieldAttributes.Create;

  FieldName1 := 'A';
  SomeLength := MockUtils.WithIntegerRange(0, 100).SomeInteger;
  SomeLabel := MockUtils.SomeString;
  SomeReadOnly := MockUtils.SomeBoolean;
  SomeMask :=  MockUtils.SomeString;
  SomeVisible := MockUtils.SomeBoolean;
  Attributes.SetAttribute(FieldName1, SomeLength, SomeLabel, SomeReadOnly, SomeMask, SomeVisible);

  FieldName2 := 'C';
  SomeLength := MockUtils.WithIntegerRange(0, 100).SomeInteger;
  SomeLabel := MockUtils.SomeString;
  SomeReadOnly := MockUtils.SomeBoolean;
  SomeMask :=  MockUtils.SomeString;
  SomeVisible := MockUtils.SomeBoolean;
  Attributes.SetAttribute(FieldName2, SomeLength, SomeLabel, SomeReadOnly, SomeMask, SomeVisible);

  FieldName3 := 'B';
  SomeLength := MockUtils.WithIntegerRange(0, 100).SomeInteger;
  SomeLabel := MockUtils.SomeString;
  SomeReadOnly := MockUtils.SomeBoolean;
  SomeMask :=  MockUtils.SomeString;
  SomeVisible := MockUtils.SomeBoolean;
  Attributes.SetAttribute(FieldName3, SomeLength, SomeLabel, SomeReadOnly, SomeMask, SomeVisible);

  FieldName4 := 'D';
  SomeLength := MockUtils.WithIntegerRange(0, 100).SomeInteger;
  SomeLabel := MockUtils.SomeString;
  SomeReadOnly := MockUtils.SomeBoolean;
  SomeMask :=  MockUtils.SomeString;
  SomeVisible := MockUtils.SomeBoolean;
  Attributes.SetAttribute(FieldName4, SomeLength, SomeLabel, SomeReadOnly, SomeMask, SomeVisible);

  with Attributes.GetFieldNamesEnumerator do
  begin
    MoveNext;
    Assert.AreEqual(FieldName1, Current);
    MoveNext;
    Assert.AreEqual(FieldName2, Current);
    MoveNext;
    Assert.AreEqual(FieldName3, Current);
    MoveNext;
    Assert.AreEqual(FieldName4, Current);
  end;
end;

procedure TFieldViewAttributesTest.SetFieldViewAttributeAddsANewAttributeWhenFieldNameIsNotYetPresent;
var
  Attributes: IDatasetFieldAttributes;
  FieldAttribute: TDatasetFieldAttribute;

  SomeFieldName: string;
  SomeLength: Integer;
  SomeLabel: string;
  SomeReadOnly: Boolean;
  SomeMask: string;
  SomeVisible: Boolean;
begin
  SomeFieldName := MockUtils.SomeString;
  SomeLength := MockUtils.WithIntegerRange(0, 100).SomeInteger;
  SomeLabel := MockUtils.SomeString;
  SomeReadOnly := MockUtils.SomeBoolean;
  SomeMask :=  MockUtils.SomeString;
  SomeVisible := MockUtils.SomeBoolean;

  Attributes := TDatasetFieldAttributes.Create;
  Attributes.SetAttribute(SomeFieldName, SomeLength, SomeLabel, SomeReadOnly, SomeMask, SomeVisible);

  Assert.AreEqual(True, Attributes.TryGetAttribute(SomeFieldName, FieldAttribute));
  Assert.AreEqual(SomeLength, FieldAttribute.Width);
  Assert.AreEqual(SomeLabel, FieldAttribute.Title);
  Assert.AreEqual(SomeReadOnly, FieldAttribute.ReadOnly);
  Assert.AreEqual(SomeMask, FieldAttribute.EditMask);
  Assert.AreEqual(SomeVisible, FieldAttribute.Visible);
end;

procedure TFieldViewAttributesTest.SetFieldViewAttributeUpdatesSameAttributeWhenFieldNameIsAlreadyPresent;
var
  Attributes: IDatasetFieldAttributes;
  FieldAttribute: TDatasetFieldAttribute;

  SomeFieldName: string;
  SomeLength: Integer;
  SomeLabel: string;
  SomeReadOnly: Boolean;
  SomeMask: string;
  SomeVisible: Boolean;
  Count: Integer;
begin
  SomeFieldName := MockUtils.SomeString;
  SomeLength := MockUtils.WithIntegerRange(0, 100).SomeInteger;
  SomeLabel := MockUtils.SomeString;
  SomeReadOnly := MockUtils.SomeBoolean;
  SomeMask :=  MockUtils.SomeString;
  SomeVisible := MockUtils.SomeBoolean;

  Attributes := TDatasetFieldAttributes.Create;
  Attributes.SetAttribute(SomeFieldName, SomeLength, SomeLabel, SomeReadOnly, SomeMask, SomeVisible);

  SomeLength := MockUtils.WithIntegerRange(0, 100).SomeInteger;
  SomeLabel := MockUtils.SomeString;
  SomeReadOnly := MockUtils.SomeBoolean;
  SomeMask :=  MockUtils.SomeString;
  SomeVisible := MockUtils.SomeBoolean;

  Attributes.SetAttribute(SomeFieldName, SomeLength, SomeLabel, SomeReadOnly, SomeMask, SomeVisible);

  Count := 0;
  with Attributes.GetFieldNamesEnumerator do
    while MoveNext do
      Inc(Count);

  Assert.AreEqual(1, Count);
  Assert.AreEqual(True, Attributes.TryGetAttribute(SomeFieldName, FieldAttribute));
  Assert.AreEqual(SomeLength, FieldAttribute.Width);
  Assert.AreEqual(SomeLabel, FieldAttribute.Title);
  Assert.AreEqual(SomeReadOnly, FieldAttribute.ReadOnly);
  Assert.AreEqual(SomeMask, FieldAttribute.EditMask);
  Assert.AreEqual(SomeVisible, FieldAttribute.Visible);
end;

initialization
  TDUnitX.RegisterTestFixture(TFieldViewAttributesTest);
end.
