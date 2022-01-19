unit Fido.Db.Connections.UniDac.PerThread;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Uni,

  Fido.Collections.PerThreadDictionary,
  Fido.Collections.PerXDictionary.Intf,

  Fido.Db.Connections.UniDac;

type
  TUniDacPerThreadConnections = class(TUniDacConnections)
  public
    constructor Create(const Parameters: TUniDacParams);
  end;

implementation

{ TUniDacPerThreadConnections }

constructor TUniDacPerThreadConnections.Create(const Parameters: TUniDacParams);
begin
  inherited Create(
    Parameters,
    function(Ownership: TDictionaryOwnerships; ValueFactoryFunc: TFunc<TUniConnection>): IPerXDictionary<TUniConnection>
    begin
      Result := TPerThreadDictionary<TUniConnection>.Create(Ownership, ValueFactoryFunc);
    end);
end;

end.
