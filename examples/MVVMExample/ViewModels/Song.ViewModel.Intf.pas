unit Song.ViewModel.Intf;

interface

uses
  Fido.DesignPatterns.Observable.Intf,

  Song.DomainObject.Intf;

type
  ISongViewModel = interface(IObservable)
  ['{A247D70A-BFC4-4CA6-B76E-8063C936F068}']
    function Title: string;
    procedure SetTitle(const Title: string);

    procedure Save;
  end;

implementation

end.
