unit Song.Repository.Data.Intf;

interface

uses
  Spring.Collections,

  Fido.VirtualQuery.Intf,
  Fido.VirtualQuery.Attributes,
  Fido.VirtualStatement.Intf,
  Fido.VirtualStatement.Attributes;


type

  ISongRecord = interface(IInvokable)
    ['{8B681FE2-3C64-4D58-9DF6-0E9561B17E03}']

    function Id: Integer;
    function Title: string;
  end;

  [SQLResource('SQL_SONG_GetList')]
  ISongListQuery = interface(IVirtualQuery)
    ['{731740A3-5B8E-46B2-9E96-53A366CB7B54}']

    function Open: IReadonlyList<ISongRecord>;
  end;

  [SQLResource('SQL_SONG_Get')]
  ISongByIdQuery = interface(IVirtualQuery)
    ['{0A3B473B-C3A1-4473-A553-8F094927182F}']

    function Open(const Id: Integer): IReadonlyList<ISongRecord>;
  end;

  [Statement(stCommand, 'SQL_SONG_Update')]
  ISongUpdateByIdCommand = interface(IVirtualStatement)
    ['{8ACAA604-8182-4FE3-B205-BBE2153B87B8}']
    function Update(const Id: Integer; [Column('title')] const Name: string): Integer;
  end;

implementation

end.
