unit Song.DomainObject;

interface

uses
  Spring,

  Fido.DesignPatterns.Observer.Notification,
  Fido.DesignPatterns.Observable,

  Song.DomainObject.Intf;

type
  TSong = class(TObservable, ISong)
  private
    FId: Integer;
    FTitle: string;

  public
    constructor Create;

    function GetId: Integer;
    procedure SetId(const Id: Integer);

    function GetTitle: string;
    procedure SetTitle(const Title: string);
  end;

implementation

{ TSong }

constructor TSong.Create;
begin
  inherited Create(Self);
end;

function TSong.GetId: Integer;
begin
  Result := FId;
end;

function TSong.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TSong.SetId(const Id: Integer);
begin
  if FId = Id then
    Exit;

  FId := Id;

  Broadcast(
    TNotification.Create(
      Self,
      'Id changed',
      TValue.From<Integer>(Id)));
end;

procedure TSong.SetTitle(const Title: string);
begin
  if FTitle = Title then
    Exit;

  FTitle := Title;

  Broadcast(
    TNotification.Create(
      Self,
      'Title changed',
      TValue.From<string>(Title)));
end;

end.
