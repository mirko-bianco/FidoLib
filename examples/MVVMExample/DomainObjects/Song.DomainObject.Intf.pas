unit Song.DomainObject.Intf;

interface

uses
  Fido.DesignPatterns.Observable.Intf;

type
  ISong = interface(IObservable)
  ['{B2960A14-103D-42FE-BAB2-C22EE7DF37FD}']
    function GetId: Integer;
    procedure SetId(const Id: Integer);

    function GetTitle: string;
    procedure SetTitle(const Title: string);

    property Id: Integer read GetId write SetId;
    property Title: string read GetTitle write SetTitle;
  end;

implementation

end.
