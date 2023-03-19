unit Person;

interface

uses
  Fido.DesignPatterns.Observable.Intf,
  Fido.DesignPatterns.Observable.Delegated;

type
  IPerson = interface(IObservable)
    ['{059DE00F-087F-41A2-9B36-B3A32C03CBA6}']

    procedure SetId(const Value: TGuid);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    function Id: TGuid;
    function FirstName: string;
    function LastName: string;
  end;

  TPerson = class(TDelegatedObservable, IPerson)
  private
    FId: TGuid;
    FFirstName: string;
    FLastName: string;
  public
    constructor Create(const Id: TGuid; const FirstName: string; const LastName: string);

    procedure SetId(const Value: TGuid);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    function Id: TGuid;
    function FirstName: string;
    function LastName: string;
  end;

implementation

{ TPerson }

constructor TPerson.Create(
  const Id: TGuid;
  const FirstName: string;
  const LastName: string);
begin
  inherited Create(nil, 'Person'+FirstName+LastName);

  Self.FId := Id;
  Self.FFirstName := FirstName;
  Self.FLastName := LastName;
end;

function TPerson.FirstName: string;
begin
  Result := FFirstName;
end;

function TPerson.Id: TGuid;
begin
  Result := FId;
end;

function TPerson.LastName: string;
begin
  Result := FLastName;
end;

procedure TPerson.SetFirstName(const Value: string);
begin
  if FFirstName = Value then
    Exit;
  FFirstName := Value;
  Self.Broadcast('FirstName changed');
end;

procedure TPerson.SetId(const Value: TGuid);
begin
  if FId = Value then
    Exit;
  FId := Value;
  Self.Broadcast('Id changed');
end;

procedure TPerson.SetLastName(const Value: string);
begin
  if FLastName = Value then
    Exit;
  FLastName := Value;
  Self.Broadcast('LastName changed');
end;

end.
