unit Tests.VirtualDTO.JSON;

interface
uses
  DUnitX.TestFramework,
  System.Json,
  System.SysUtils,
  System.DateUtils,
  System.TypInfo,

  Spring,
  Spring.Collections,

  Fido.Testing.Mock.Utils,
  Fido.JSON.Marshalling;

type
  TMediaType = (mtLP, mtCassette, mtCD, mtOnline);

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

  [TestFixture]
  TVirtualDtoJSONTests = class(TObject)
  public
    [Test]
    procedure TryCreateObjectWorksWithAString;
  end;

implementation


{ TVirtualDtoJSONTests }

procedure TVirtualDtoJSONTests.TryCreateObjectWorksWithAString;
var
  JSONObject: Shared<TJSONObject>;
  JSONObject2: TJSONObject;
  JSONObject3: TJSONObject;
  JSONObject4: TJSONObject;
  JSONArray: TJSONArray;
  JSONArray2: TJSONArray;
  Song: ISong;
  ReleaseDate: TDateTime;
begin
  JSONObject := TJSONObject.Create;
  JSONObject.Value.AddPair('Id', TJSONNumber.Create(1));
  JSONObject.Value.AddPair('Guid', TJSONString.Create('F76CD4D4-35E1-4C66-A30A-37C050C0B324'));
  JSONObject.Value.AddPair('Title', TJSONString.Create('My Title'));
  JSONObject.Value.AddPair('IsGood', TJSONFalse.Create);
  ReleaseDate := Now;
  JSONObject.Value.AddPair('ReleaseDate', TJSONString.Create(DateToISO8601(ReleaseDate)));
  JSONObject.Value.AddPair('Price', TJSONString.Create('15.95'));

  JSONObject2 := TJSONObject.Create;
  JSONObject2.AddPair('Id', TJSONNumber.Create(2));
  JSONObject2.AddPair('Name', TJSONString.Create('Author name'));

  JSONObject.Value.AddPair('Author', JSONObject2);

  JSONArray := TJSONArray.Create;
  JSONArray.AddElement(TJSONNumber.Create(2001));
  JSONArray.AddElement(TJSONNumber.Create(2002));
  JSONObject.Value.AddPair('Years', JSONArray);

  JSONArray2 := TJSONArray.Create;

  JSONObject3 := TJSONObject.Create;
  JSONObject3.AddPair('Id', TJSONNumber.Create(3));
  JSONObject3.AddPair('Name', TJSONString.Create('First singer'));

  JSONObject4 := TJSONObject.Create;
  JSONObject4.AddPair('Id', TJSONNumber.Create(4));
  JSONObject4.AddPair('Name', TJSONString.Create('Second singer'));

  JSONArray2.AddElement(JSONObject3);
  JSONArray2.AddElement(JSONObject4);
  JSONObject.Value.AddPair('Singers', JSONArray2);

  JSONObject.Value.AddPair('Media', TJSONNumber.Create(Integer(mtCD)));

  Song := TJSONVirtualDto.Create(TypeInfo(ISong), JSONObject) as ISong;

  Assert.AreEqual(1, Song.Id);
  Assert.AreEqual(StringToGuid('{F76CD4D4-35E1-4C66-A30A-37C050C0B324}'), Song.Guid);
  Assert.AreEqual('My Title', Song.Title);
  Assert.AreEqual(False, Song.IsGood);
  Assert.AreEqual(ReleaseDate, Song.ReleaseDate.Value);
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

initialization
  TDUnitX.RegisterTestFixture(TVirtualDtoJSONTests);
end.
