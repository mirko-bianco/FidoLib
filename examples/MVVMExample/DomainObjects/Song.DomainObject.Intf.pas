unit Song.DomainObject.Intf;

interface

uses
  Fido.DesignPatterns.Observable.Intf;

type
  {$M+}
  ISong = interface(IObservable)
  ['{B2960A14-103D-42FE-BAB2-C22EE7DF37FD}']
    function Id: Integer;
    procedure SetId(const Id: Integer);

    function Title: string;
    procedure SetTitle(const Title: string);
  end;

implementation

end.
