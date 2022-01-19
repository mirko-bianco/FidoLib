unit Fido.Db.Connections.UniDac.PerIWSession;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Uni,

  Fido.Collections.PerIWSessionDictionary,
  Fido.Collections.PerXDictionary.Intf,

  Fido.Db.Connections.UniDac;

type
  TUniDacPerIWSessionConnections = class(TUniDacConnections)
  public
    constructor Create(const Parameters: TUniDacParams);
  end;

implementation

{ TUniDacPerIWSessionConnections }

constructor TUniDacPerIWSessionConnections.Create(const Parameters: TUniDacParams);
begin
  inherited Create(
    Parameters,
    function(Ownership: TDictionaryOwnerships; ValueFactoryFunc: TFunc<TUniConnection>): IPerXDictionary<TUniConnection>
    begin
      Result := TPerIWSessionDictionary<TUniConnection>.Create(Ownership, ValueFactoryFunc);
    end);
end;

end.
