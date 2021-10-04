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
    function Store(const Song: ISong): ISong;
    procedure Delete(const Id: Integer);
  end;

implementation

end.
