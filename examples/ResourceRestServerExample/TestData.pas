unit TestData;

interface

uses
  Fido.Http.Types,
  Fido.Api.Server.Resource.Attributes;

type
  {$M+}
  TData = class
  private
    FName: string;
    FLastName: string;
    FAge: Integer;
  published
    property Name: string read FName write FName;
    property LastName: string read FLastName write FLastName;
    property Age: Integer read FAge write FAge;
  end;

  {$M+}
  [BaseUrl('/Test')]
  [ConsumesAttribute(mtJson)]
  [ProducesAttribute(mtJson)]
  TTestResource = class(TObject)

    [Path(rmGet, '/helloworld')]
    function HelloWorld: string;

    [Path(rmGet, '/helloworld/{name}')]
    function HelloWorldName(const [PathParam]Name: string): string;

    [Path(rmGet, '/Token')]
    procedure GetToken(out [HeaderParam] Token: string);

    [Path(rmPost, '/PostData')]
    procedure PostData(const [BodyParam] Data: TData);

    [Path(rmPost, '/OutData')]
    procedure OutData(out [BodyParam] Data: TData);

    [Path(rmGet, '/OutData')]
    function Data: TData;

    destructor Destroy; override;
  end;

implementation

{ TTestEndPoint }

function TTestResource.Data: TData;
begin
  Data := TData.Create;
  Data.Name := 'Me';
  Data.LastName := 'again me';
  Data.Age := 22;
end;

destructor TTestResource.Destroy;
begin

  inherited;
end;

procedure TTestResource.GetToken(out Token: string);
begin
  Token := '{C6A68632-28E3-410F-8B6C-F8EB7133AF8B}';
end;

function TTestResource.HelloWorld: string;
begin
  Result := 'Hello World';
end;

function TTestResource.HelloWorldName(const Name: string): string;
begin
  Result := 'Hello World and ' + Name;
end;

procedure TTestResource.OutData(out Data: TData);
begin
  Data := TData.Create;
  Data.Name := 'Me';
  Data.LastName := 'again me';
  Data.Age := 12;
end;

procedure TTestResource.PostData(const Data: TData);
begin
  writeln(Data.Name);
  writeln(Data.LastName);
  writeln(Data.Age);
end;

end.
