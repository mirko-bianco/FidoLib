unit Fido.DesignPatterns.Adapter.DataSetAsReadonlyList.Test;

interface

uses
  DUnitX.TestFramework,
  SysUtils,
  DB,

  Spring,
  Spring.Collections,

  Fido.Db.ListDatasets,
  Fido.DesignPatterns.Adapter.DataSetAsReadonlyList;

type
  IType = interface(IInvokable)
    ['{A9FD79E3-BED9-402F-8CE7-A92B80D95413}']

    function GetId: Integer;
    Function GetName: string;
  end;

  TType = class(TInterfacedObject, IType)
  strict private
    FId: Integer;
    FName: string;
  public
    constructor Create(const Id: Integer; const Name: string);

    function GetId: Integer;
    Function GetName: string;
  end;

  [TestFixture]
  TDataSetAsReadonlyListTests = class
  public
    [Test]
    procedure TDataSetAsReadonlyListWorks;
  end;

implementation

{ TType }

constructor TType.Create(const Id: Integer; const Name: string);
begin
  inherited Create;
  FId := Id;
  FName := Name;
end;

function TType.GetId: Integer;
begin
  Result := FId;
end;

function TType.GetName: string;
begin
  Result := FName;
end;

{ TDataSetAsReadonlyListTests }

procedure TDataSetAsReadonlyListTests.TDataSetAsReadonlyListWorks;
var
  List: IList<IType>;
  DataSet: Shared<TDataSet>;
  TestList: Shared<TDataSetAsReadonlyList<IType>>;
begin
  List := TCollections.CreateInterfaceList<IType>([
    TType.Create(1, 'Name1'),
    TType.Create(2, 'Name2')]);

  DataSet := ListDatasets.ListToReadOnlyDataset<IType>(List);
  TestList := TDataSetAsReadonlyList<IType>.Create(DataSet);


  Assert.AreEqual(2, Length(TestList.Value.ToArray));
  Assert.AreEqual(1, TestList.Value.ElementAt(0).GetId);
  Assert.AreEqual('Name1', TestList.Value.ElementAt(0).GetName);
  Assert.AreEqual(2, TestList.Value.ElementAt(1).GetId);
  Assert.AreEqual('Name2', TestList.Value.ElementAt(1).GetName);
end;

initialization
  TDUnitX.RegisterTestFixture(TDataSetAsReadonlyListTests);
end.
