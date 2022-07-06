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

    function EmptyArray: IReadOnlyList<string>;
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
  Json: string;
  Song: ISong;
begin
  Json := '{"Id":1,"Guid":"F76CD4D4-35E1-4C66-A30A-37C050C0B324","Title":"My Title","IsGood":false,"ReleaseDate":"2021-09-29T22:15:30.579Z","Price":"15.95","Author":{"Id":2,"Name":"Author name"},' +
    '"Years":[2001,2002],"Singers":[{"Id":3,"Name":"First singer"},{"Id":4,"Name":"Second singer"}],"Media":2, "EmptyArray": []}';

  Song := TJSONVirtualDto.Create(TypeInfo(ISong), Json) as ISong;

  Assert.AreEqual(1, Song.Id);
  Assert.AreEqual(StringToGuid('{F76CD4D4-35E1-4C66-A30A-37C050C0B324}'), Song.Guid);
  Assert.AreEqual('My Title', Song.Title);
  Assert.AreEqual(False, Song.IsGood);
  Assert.AreEqual(ISO8601ToDate('2021-09-29T22:15:30.579Z'), Song.ReleaseDate.Value);
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
  Assert.AreEqual(0, Song.EmptyArray.Count);
end;

initialization
  TDUnitX.RegisterTestFixture(TVirtualDtoJSONTests);
end.
