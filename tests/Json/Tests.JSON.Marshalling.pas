unit Tests.JSON.Marshalling;

interface
uses
  DUnitX.TestFramework,
  System.Json,
  System.SysUtils,
  System.DateUtils,
  System.TypInfo,
  System.Classes,

  Spring,
  Spring.Collections,

  Fido.Testing.Mock.Utils,
  Fido.JSON.Marshalling;

type
  TMediaType = (mtLP, mtCassette, mtCD, mtOnline);

  {$M+}
  TMyObject = class
  strict private
    FId: integer;
    FName: Nullable<string>;
  published
    property Id: Integer read FId write FId;
    property Name: Nullable<string> read FName write FName;
  end;
  {$M-}

  ISinger = interface(IInvokable)
    ['{DC5320BB-1338-4745-B958-8A0BBD74DAB5}']
    function Id: Integer;
    function Name: string;
  end;

  IAuthor = interface(IInvokable)
    ['{2E7D194B-E2F4-4859-9330-8410164B4995}']

    function Id: Integer;
    function Name: string;
  end;

  ISong = interface(IInvokable)
    ['{8B681FE2-3C64-4D58-9DF6-0E9561B17E03}']

    function Id: Integer;
    function Guid: TGuid;
    function Title: string;
    function IsGood: Boolean;
    function ReleaseDate: Nullable<TDateTime>;
    function RetirementDate: Nullable<TDateTime>;
    function Price: Currency;

    function Author: IAuthor;

    function Years: IReadOnlyList<Integer>;
    function Singers: IReadOnlyList<ISinger>;

    function Media: TMediaType;
  end;

  ISongReadOnlyList = IReadOnlyList<ISong>;

  [TestFixture]
  TJSONMarshallingTests = class(TObject)
  public
    [Test]
    procedure JSONUnmarshallingToEnumeration;

    [Test]
    procedure JSONMarshallingFromEnumeration;

    [Test]
    procedure JSONUnmarshallingToInteger;

    [Test]
    procedure JSONMarshallingFromInteger;

    [Test]
    procedure JSONUnmarshallingToNullableInteger;

    [Test]
    procedure JSONMarshallingFromNullableInteger;

    [Test]
    procedure JSONUnmarshallingToInt64;

    [Test]
    procedure JSONMarshallingFromInt64;

    [Test]
    procedure JSONUnmarshallingToNullableInt64;

    [Test]
    procedure JSONMarshallingFromNullableInt64;

    [Test]
    procedure JSONUnmarshallingToString;

    [Test]
    procedure JSONUnmarshallingToNullableString;

    [Test]
    procedure JSONUnmarshallingToDateTime;

    [Test]
    procedure JSONUnmarshallingToNullableDateTime;

    [Test]
    procedure JSONUnmarshallingToDouble;

    [Test]
    procedure JSONUnmarshallingToNullableDouble;

    [Test]
    procedure JSONUnmarshallingToExtended;

    [Test]
    procedure JSONUnmarshallingToNullableExtended;

    [Test]
    procedure JSONUnmarshallingToCurrency;

    [Test]
    procedure JSONUnmarshallingToNullableCurrency;

    [Test]
    procedure JSONUnmarshallingToBoolean;

    [Test]
    procedure JSONUnmarshallingToNullableBoolean;

    [Test]
    procedure JSONUnmarshallingToGuid;

    [Test]
    procedure JSONUnmarshallingToNullableGuid;

    [Test]
    procedure JSONUnmarshallingToSmallint;

    [Test]
    procedure JSONUnmarshallingToNullableSmallint;

    [Test]
    procedure JSONUnmarshallingToObject;

    [Test]
    procedure JSONUnmarshallingToInterface;

    [Test]
    procedure JSONUnmarshallingToIntegerReadonlyList;

    [Test]
    procedure JSONUnmarshallingToInterfaceReadonlyList;
  end;

implementation


{ TJSONMarshallingTests }

procedure TJSONMarshallingTests.JSONMarshallingFromEnumeration;
begin
  Assert.AreEqual('1', JSONMarshaller.&From<TMediaType>(mtCassette));
end;

procedure TJSONMarshallingTests.JSONMarshallingFromInt64;
begin
  Assert.AreEqual('9223372036854775807', JSONMarshaller.From<Int64>(9223372036854775807));
end;

procedure TJSONMarshallingTests.JSONMarshallingFromInteger;
begin
  Assert.AreEqual('2147483647', JSONMarshaller.From<Integer>(2147483647));
end;

procedure TJSONMarshallingTests.JSONMarshallingFromNullableInt64;
var
  NullableNull: Nullable<Int64>;
begin
  Assert.AreEqual('9223372036854775807', JSONMarshaller.From<Nullable<Int64>>(9223372036854775807));

  Assert.AreEqual('Null', JSONMarshaller.From<Nullable<Int64>>(NullableNull));
end;

procedure TJSONMarshallingTests.JSONMarshallingFromNullableInteger;
var
  NullableNull: Nullable<Integer>;
begin
  Assert.AreEqual('2147483647', JSONMarshaller.From<Nullable<Integer>>(2147483647));

  Assert.AreEqual('Null', JSONMarshaller.From<Nullable<Integer>>(NullableNull));
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToBoolean;
var
  JSONBool: Shared<TJSONBool>;
begin
  JSONBool := TJSONBool.Create(True);
  Assert.AreEqual(True, JSONUnmarshaller.&To<Boolean>(JSONBool.Value.ToJSON));

  JSONBool := TJSONBool.Create(False);
  Assert.AreEqual(False, JSONUnmarshaller.&To<Boolean>(JSONBool.Value.ToJSON));
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToCurrency;
begin
  Assert.AreEqual<Currency>(13.6575, JSONUnmarshaller.&To<Currency>('13.6575'));
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToDateTime;
var
  Dt: TDateTime;
begin
  Dt := Now;
  Assert.AreEqual<TDateTime>(Dt, JSONUnmarshaller.&To<TDateTime>(DateToISO8601(Dt)));
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToDouble;
begin
  Assert.AreEqual<Double>(13.65754674, JSONUnmarshaller.&To<Double>('13.65754674'));
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToEnumeration;
begin
  Assert.AreEqual(mtCassette, JSONUnmarshaller.&To<TMediaType>('1'));
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToExtended;
begin
  Assert.AreEqual(13.65754674, JSONUnmarshaller.&To<Extended>('13.65754674'));
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToGuid;
var
  Guid: TGuid;
begin
  Guid := TGuid.NewGuid;
  Assert.AreEqual<TGuid>(Guid, JSONUnmarshaller.&To<TGuid>(StringReplace(StringReplace(GUIDToString(Guid), '{', '', []), '}', '', [])));
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToInt64;
begin
  Assert.AreEqual(9223372036854775807, JSONUnmarshaller.&To<Int64>('9223372036854775807'));
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToInteger;
begin
  Assert.AreEqual(2147483647, JSONUnmarshaller.&To<Integer>('2147483647'));
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToIntegerReadonlyList;
var
  List: IReadOnlyList<Integer>;
begin
  List := JSONUnmarshaller.&To<IReadOnlyList<Integer>>('[1, 2, 3, 4]');

  Assert.AreEqual(4, List.Count);
  Assert.AreEqual(1, List[0]);
  Assert.AreEqual(2, List[1]);
  Assert.AreEqual(3, List[2]);
  Assert.AreEqual(4, List[3]);
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToInterface;
var
  Song: ISong;
begin
  Song := JSONUnmarshaller.&To<ISong>(
    '{"Id":1,"Guid":"F76CD4D4-35E1-4C66-A30A-37C050C0B324","Title":"My Title","IsGood":false,"ReleaseDate":"2021-09-16T16:59:07.096Z","Price":"15.95",' +
    '"Author":{"Id":2,"Name":"Author name"},"Years":[2001,2002],"Singers":[{"Id":3,"Name":"First singer"},{"Id":4,"Name":"Second singer"}],"Media":2}');

  Assert.AreEqual(1, Song.Id);
  Assert.AreEqual(StringToGuid('{F76CD4D4-35E1-4C66-A30A-37C050C0B324}'), Song.Guid);
  Assert.AreEqual('My Title', Song.Title);
  Assert.AreEqual(False, Song.IsGood);
  Assert.AreEqual(ISO8601ToDate('2021-09-16T16:59:07.096Z'), Song.ReleaseDate.Value);
  Assert.AreEqual(False, Song.RetirementDate.HasValue);
  Assert.AreEqual<Currency>(15.95, Song.Price);
  Assert.AreEqual('Author name', song.Author.Name);
  Assert.AreEqual(2, song.Author.Id);
  Assert.AreEqual(3, song.Singers[0].Id);
  Assert.AreEqual('First singer', song.Singers[0].Name);
  Assert.AreEqual(4, song.Singers[1].Id);
  Assert.AreEqual('Second singer', song.Singers[1].Name);
  Assert.AreEqual(2001, song.Years[0]);
  Assert.AreEqual(2002, song.Years[1]);
  Assert.AreEqual(mtCD, song.Media);
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToInterfaceReadonlyList;
var
  List: IReadOnlyList<ISong>;
begin
  List := JSONUnmarshaller.&To<IReadOnlyList<ISong>>(
    '[' +
    '{"Id":1,"Guid":"F76CD4D4-35E1-4C66-A30A-37C050C0B324","Title":"My Title","IsGood":false,"ReleaseDate":"2021-09-16T16:59:07.096Z","Price":"15.95",' +
    '"Author":{"Id":2,"Name":"Author name"},"Years":[2001,2002],"Singers":[{"Id":3,"Name":"First singer"},{"Id":4,"Name":"Second singer"}],"Media":2},'+
    '{"Id":2,"Guid":"F76CD4D4-35E1-4C66-A30A-37C050C0B324","Title":"My Title","IsGood":false,"ReleaseDate":"2021-09-16T16:59:07.096Z","Price":"15.95",' +
    '"Author":{"Id":2,"Name":"Author name"},"Years":[2001,2002],"Singers":[{"Id":3,"Name":"First singer"},{"Id":4,"Name":"Second singer"}],"Media":2},'+
    '{"Id":3,"Guid":"F76CD4D4-35E1-4C66-A30A-37C050C0B324","Title":"My Title","IsGood":false,"ReleaseDate":"2021-09-16T16:59:07.096Z","Price":"15.95",' +
    '"Author":{"Id":2,"Name":"Author name"},"Years":[2001,2002],"Singers":[{"Id":3,"Name":"First singer"},{"Id":4,"Name":"Second singer"}],"Media":2},'+
    '{"Id":4,"Guid":"F76CD4D4-35E1-4C66-A30A-37C050C0B324","Title":"My Title","IsGood":false,"ReleaseDate":"2021-09-16T16:59:07.096Z","Price":"15.95",' +
    '"Author":{"Id":2,"Name":"Author name"},"Years":[2001,2002],"Singers":[{"Id":3,"Name":"First singer"},{"Id":4,"Name":"Second singer"}],"Media":2}'+
    ']');

  Assert.AreEqual(4, List.Count);
  Assert.AreEqual(1, List[0].Id);
  Assert.AreEqual(2, List[1].Id);
  Assert.AreEqual(3, List[2].Id);
  Assert.AreEqual(4, List[3].Id);
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToNullableBoolean;
var
  JSONBool: Shared<TJSONBool>;
begin
  JSONBool := TJSONBool.Create(True);
  Assert.AreEqual(True, JSONUnmarshaller.&To<Nullable<Boolean>>(JSONBool.Value.ToJSON).Value);

  JSONBool := TJSONBool.Create(False);
  Assert.AreEqual(False, JSONUnmarshaller.&To<Nullable<Boolean>>(JSONBool.Value.ToJSON).Value);

  Assert.AreEqual(False, JSONUnmarshaller.&To<Nullable<Boolean>>('Null').HasValue);
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToNullableCurrency;
begin
  Assert.AreEqual<Currency>(13.6575, JSONUnmarshaller.&To<Nullable<Currency>>('13.6575'));

  Assert.AreEqual(False, JSONUnmarshaller.&To<Nullable<Currency>>('Null').HasValue);
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToNullableDateTime;
var
  Dt: TDateTime;
begin
  Dt := Now;
  Assert.AreEqual<TDateTime>(Dt, JSONUnmarshaller.&To<Nullable<TDateTime>>(DateToISO8601(Dt)).Value);

  Assert.AreEqual(False, JSONUnmarshaller.&To<Nullable<TDateTime>>('Null').HasValue);
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToNullableDouble;
begin
  Assert.AreEqual<Double>(13.65754674, JSONUnmarshaller.&To<Nullable<Double>>('13.65754674').Value);

  Assert.AreEqual(False, JSONUnmarshaller.&To<Nullable<Double>>('Null').HasValue);
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToNullableExtended;
begin
  Assert.AreEqual(13.65754674, JSONUnmarshaller.&To<Nullable<Extended>>('13.65754674').Value);

  Assert.AreEqual(False, JSONUnmarshaller.&To<Nullable<Extended>>('Null').HasValue);
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToNullableGuid;
var
  Guid: TGuid;
begin
  Guid := TGuid.NewGuid;
  Assert.AreEqual<TGuid>(Guid, JSONUnmarshaller.&To<Nullable<TGuid>>(StringReplace(StringReplace(GUIDToString(Guid), '{', '', []), '}', '', [])).Value);

  Assert.AreEqual(False, JSONUnmarshaller.&To<Nullable<TGuid>>('Null').HasValue);
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToNullableInt64;
begin
  Assert.AreEqual<Int64>(9223372036854775807, JSONUnmarshaller.&To<Nullable<Int64>>('9223372036854775807').Value);

  Assert.AreEqual(False, JSONUnmarshaller.&To<Nullable<Int64>>('Null').HasValue);
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToNullableInteger;
begin
  Assert.AreEqual<Integer>(2147483647, JSONUnmarshaller.&To<Nullable<Integer>>('2147483647').Value);

  Assert.AreEqual(False, JSONUnmarshaller.&To<Nullable<Integer>>('Null').HasValue);
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToNullableSmallint;
begin
  Assert.AreEqual<Smallint>(25000, JSONUnmarshaller.&To<Nullable<Smallint>>('25000').Value);

  Assert.AreEqual(False, JSONUnmarshaller.&To<Nullable<Smallint>>('Null').HasValue);
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToNullableString;
begin
  Assert.AreEqual('a string', JSONUnmarshaller.&To<Nullable<string>>('a string').Value);

  Assert.AreEqual(False, JSONUnmarshaller.&To<Nullable<string>>('Null').HasValue);
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToObject;
var
  Obj: Shared<TMyObject>;
begin
  Obj := JSONUnmarshaller.&To<TMyObject>('{"Id":"1","Name":"Null"}');
  Assert.AreEqual(1, Obj.Value.Id);
  Assert.AreEqual(False, Obj.Value.Name.HasValue);
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToSmallint;
begin
  Assert.AreEqual<Smallint>(25000, JSONUnmarshaller.&To<Smallint>('25000'));
end;

procedure TJSONMarshallingTests.JSONUnmarshallingToString;
begin
  Assert.AreEqual('a string', JSONUnmarshaller.&To<string>('a string'));
end;

initialization
  TDUnitX.RegisterTestFixture(TJSONMarshallingTests);
end.
