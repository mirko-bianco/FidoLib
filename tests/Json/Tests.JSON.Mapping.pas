unit Tests.JSON.Mapping;

interface

uses
  DUnitX.TestFramework,
  System.Json,
  System.SysUtils,
  System.DateUtils,
  System.TypInfo,
  System.Classes,
  System.Rtti,

  Spring,
  Spring.Collections,

  Fido.Types,
  Fido.Testing.Mock.Utils,
  Fido.JSON.Mapping,
  Fido.JSON.Marshalling;

type
  {$M+}
  TMyObject = class
  strict private
    FId: integer;
    FName: Nullable<string>;
  public
    function ToJson: string; virtual;
  published
    property Id: Integer read FId write FId;
    property Name: Nullable<string> read FName write FName;
  end;

  TMyDescObject = class(TMyObject)
  strict private
    FBirthDate: TDateTime;
  public
    function ToJson: string; override;
  published
    property BirthDate: TDateTime read FBirthDate write FBirthDate;
  end;
  {$M-}

  [TestFixture]
  TJSONMappingTests = class(TObject)
  public
    [Test]
    procedure MarshallingADescendentClassWorks;

    [Test]
    procedure UnMarshallingADescendentClassWorks;
  end;

implementation


{ TJSONMappingTests }

procedure TJSONMappingTests.MarshallingADescendentClassWorks;
var
  MyDescObject: Shared<TMyDescObject>;
begin
  MappingsUtilities.RegisterType<TMyObject>(
    function(const Value: TMyObject): string
    begin
      Result := Value.ToJson;
    end,
    function(const Value: string; const TypInfo: pTypeInfo): TMyObject
    var
      RttiContext: TRttiContext;
      RttiType: TRttiType;
      InstanceType: TRttiInstanceType;
    begin
      RttiContext := TRttiContext.Create;
      RttiType := RttiContext.GetType(TypInfo);
      InstanceType := RttiType.AsInstance;
      Result := InstanceType.GetMethod('Create').Invoke(InstanceType.MetaclassType, []).AsType<TMyObject>;
      Result.Name := 'ItWorked';
    end);

  MyDescObject := TMyDescObject.Create;
  MyDescObject.Value.BirthDate := EncodeDateDay(2022, 1);
  MyDescObject.Value.Id := 100;
  MyDescObject.Value.Name := 'Mirko';

  Assert.AreEqual('{"ID":100,"Name":"Mirko","BirthDate":"1\/1\/2022"}', JSONMarshaller.From(MyDescObject.Value));
end;

procedure TJSONMappingTests.UnMarshallingADescendentClassWorks;
var
  MyDescObject: Shared<TMyDescObject>;
begin
  MappingsUtilities.RegisterType<TMyObject>(
    function(const Value: TMyObject): string
    begin
      Result := '{"Name":"It Worked"}';
    end,
    function(const Value: string; const TypInfo: pTypeInfo): TMyObject
    var
      RttiContext: TRttiContext;
      RttiType: TRttiType;
      InstanceType: TRttiInstanceType;
    begin
      RttiContext := TRttiContext.Create;
      RttiType := RttiContext.GetType(TypInfo);
      InstanceType := RttiType.AsInstance;
      Result := InstanceType.GetMethod('Create').Invoke(InstanceType.MetaclassType, []).AsType<TMyObject>;
      Result.Name := 'ItWorked';
    end);

  MyDescObject := JSONUnmarshaller.To<TMyDescObject>('{"Name":"Test"}');

  Assert.AreEqual<string>('ItWorked', MyDescObject.Value.Name);
end;

{ TMyObject }

function TMyObject.ToJson: string;
begin
  Result := Format('{"ID": %d, "Name":"%s"}', [Self.Id, Self.Name.Value]);
end;

{ TMyDescObject }

function TMyDescObject.ToJson: string;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Self.BirthDate, Year, Month, Day);
  Result := Format('{"ID": %d, "Name":"%s", "BirthDate": "%d/%d/%d"}', [Self.Id, Self.Name.Value, Day, Month, Year]);
end;

initialization
  TDUnitX.RegisterTestFixture(TJSONMappingTests);
end.
