unit Song.Repository.Intf;

interface

uses
  Fido.Collections.DeepObservableList.Intf,

  Song.DomainObject.Intf;

type
  ISongRepository = interface(IInvokable)
  ['{26981712-6D27-4003-831A-B384B2BF28F3}']

    function GetNew: ISong;
    function GetList: IDeepObservableList<ISong>;
    function GetById(const Id: Integer): ISong;
    procedure Update(const Song: ISong);
  end;

implementation

end.
