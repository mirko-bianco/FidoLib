unit Fido.Collections.UpdateablePerXDictionary.Intf;

interface

type
  IUpdatablePerXDictionary<T, TUpdateable> = Interface(IInvokable)
    ['{1DA7B26E-0E6C-436D-9DDF-B3E36C9B7063}']
    procedure SetUpdateableValue(const Updateable: TUpdateable);
    function GetUpdateableValue: TUpdateable;

    function GetCurrent: T;
    procedure ReleaseCurrent;
    property UpdateableValue: TUpdateable read GetUpdateableValue
      write SetUpdateableValue;
  End;

implementation

end.
