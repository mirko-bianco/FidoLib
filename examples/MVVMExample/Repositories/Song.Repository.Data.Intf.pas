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

    function Execute(const Id: Integer; [Column('title')] const Name: string): Integer;
  end;

  [Statement(stCommand, 'SQL_SONG_Insert')]
  ISongInsertCommand = interface(IVirtualStatement)
    ['{F2482C88-AC0A-420E-983D-4DF60E4E1FEF}']

    function Execute([Column('title')] const Name: string): Integer;
  end;

  IIdRecord = interface(IInvokable)
    ['{BF8168FE-B3F3-48C8-9BD7-FAAAE2ADDCAB}']

    function Id: Integer;
  end;

  [SQLResource('SQL_SONG_LastId')]
  ISongLastIdQuery = interface(IVirtualQuery)
    ['{02061EC0-76B1-46BE-AF8D-D7898B5785AD}']

    function Execute: IReadonlyList<IIdRecord>;
  end;

  [Statement(stCommand, 'SQL_SONG_Delete')]
  ISongDeleteByIdCommand = interface(IVirtualStatement)
    ['{37BF07AF-09C0-4ED5-A61B-6940ABE62377}']

    function Execute(const Id: Integer): Integer;
  end;

implementation

end.
