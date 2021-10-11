unit Fido.DesignPatterns.Adapter.JsonArrayAsReadonlyList.Test;

interface

uses
  DUnitX.TestFramework,
  SysUtils,
  DB,
  JSON,


  Spring,
  Spring.Collections,

  Fido.Db.ListDatasets,
  Fido.DesignPatterns.Adapter.JsonArrayAsReadonlyList;

type
  IType = interface(IInvokable)
    ['{A9FD79E3-BED9-402F-8CE7-A92B80D95413}']

    function GetId: Integer;
    Function GetName: string;
  end;

  [TestFixture]
  TJsonArrayAsReadonlyListTests = class
  public
    [Test]
    procedure TJsonArrayAsReadonlyListWorks;
  end;

implementation

{ TJsonArrayAsReadonlyListTests }

procedure TJsonArrayAsReadonlyListTests.TJsonArrayAsReadonlyListWorks;
var
  JsonArray: Shared<TJsonArray>;
  TestList: Shared<TJsonArrayAsReadonlyList<IType>>;
begin
  JsonArray := TJSONObject.ParseJSONValue('[{"Id": "1", "Name": "Name1"}, {"Id": "2", "Name": "Name2"}]') as TJSONArray;

  TestList := TJsonArrayAsReadonlyList<IType>.Create(JsonArray);


  Assert.AreEqual(2, Length(TestList.Value.ToArray));
  Assert.AreEqual(1, TestList.Value.ElementAt(0).GetId);
  Assert.AreEqual('Name1', TestList.Value.ElementAt(0).GetName);
  Assert.AreEqual(2, TestList.Value.ElementAt(1).GetId);
  Assert.AreEqual('Name2', TestList.Value.ElementAt(1).GetName);
end;

initialization
  TDUnitX.RegisterTestFixture(TJsonArrayAsReadonlyListTests);
end.

