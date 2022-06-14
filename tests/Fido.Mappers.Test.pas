unit Fido.Mappers.Test;

interface

uses
  DUnitX.TestFramework,
  SysUtils,

  Spring,

  Fido.Mappers;

type
  TFrom = class
    DateTime: TDateTime;
  end;

  TTo = class
    DateTime: string;
  end;

  {$M+}
  TAutoFrom = class
  strict private
    function GetName: string;
  published
    function Id: Integer;

    property Name: string read GetName;
  end;

  TAutoTo = class
  strict private
    FId: Integer;
    FName: string;
  published
    procedure SetName(const Name: string);

    property Id: Integer read FId write FId;
    property Name: string read FName;
  end;
  {$M-}

  [TestFixture]
  TMappersTests = class
  public
    [Test]
    procedure MapperIsStoredAndWorksCorrectly;

    [Test]
    procedure AutoMapperWorksCorrectly;
  end;

implementation

{ TMappersTests }

procedure TMappersTests.AutoMapperWorksCorrectly;
var
  From: Shared<TAutoFrom>;
  &To: TAutoTo;
begin
  From := TAutoFrom.Create;
  &To := TAutoTo.Create;
  try
    Mappers.ClearMappers;

    Mappers.Map<TAutoFrom, TAutoTo>(From.Value, &To);

    Assert.AreEqual(From.Value.Id, &To.Id);
    Assert.AreEqual(From.Value.Name, &To.Name);
  finally
    &To.Free;
  end;
end;

procedure TMappersTests.MapperIsStoredAndWorksCorrectly;
var
  From: Shared<TFrom>;
  &To: TTo;
  TestTo: string;
begin
  From := TFrom.Create;
  From.Value.DateTime := Now;
  &To := TTo.Create;
  try
    Mappers.ClearMappers;
    Mappers.RegisterMapper<TFrom, TTo>(
      procedure(const From: TFrom; var &To: TTo)
      begin
        &To.DateTime := DateTimeToStr(From.DateTime);
      end);

    TestTo := DateTimeToStr(From.Value.DateTime);
    Mappers.Map<TFrom, TTo>(From, &To);

    Assert.AreEqual(TestTo, &To.DateTime);
  finally
    &To.Free;
  end;
end;

{ TAutoFrom }

function TAutoFrom.GetName: string;
begin
  Result := 'TestName';
end;

function TAutoFrom.Id: Integer;
begin
  Result := 100;
end;

{ TAutoTo }

procedure TAutoTo.SetName(const Name: string);
begin
  FName := Name;
end;

initialization
  TDUnitX.RegisterTestFixture(TMappersTests);
end.
