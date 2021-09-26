unit Song.Model.Intf;

interface

uses
  Fido.Collections.DeepObservableList.Intf,

  Song.DomainObject.Intf;

type
  ISongModel = interface(IInvokable)
  ['{85D69DF4-D7AE-46B7-95BB-A0C09A08A676}']

    function GetList: IDeepObservableList<ISong>;

    function New: ISong;

    function Get(const Id: Integer): ISong;

    procedure Save(const Song: ISong);
  end;

implementation

end.
