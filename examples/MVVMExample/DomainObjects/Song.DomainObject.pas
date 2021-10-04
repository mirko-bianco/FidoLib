unit Song.DomainObject;

interface

uses
  Spring,

  Fido.DesignPatterns.Observer.Notification,
  Fido.DesignPatterns.Observable.Delegated,

  Song.DomainObject.Intf;

type
  TSong = class(TDelegatedObservable, ISong)
  private
    FId: Integer;
    FTitle: string;

  public
    constructor Create;

    function Id: Integer;
    procedure SetId(const Id: Integer);

    function Title: string;
    procedure SetTitle(const Title: string);
  end;

implementation

{ TSong }

constructor TSong.Create;
begin
  inherited Create(Self);
end;

function TSong.Id: Integer;
begin
  Result := FId;
end;

function TSong.Title: string;
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
