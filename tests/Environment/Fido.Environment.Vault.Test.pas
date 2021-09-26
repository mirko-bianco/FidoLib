unit Fido.Environment.Vault.Test;

interface

uses
  SysUtils,
  DUnitX.TestFramework,

  Fido.Environment.Vault.Intf,
  Fido.Environment.Vault;

type
  [TestFixture]
  TVaultTests = class
  private
    function UniqueId: String;
  public
    [Test]
    procedure WhenFetchingASecret_IfItExistsForOtherEnvironmentOnly_EmptyValueIsReturned;
    [Test]
    procedure WhenFetchingASecret_IfItExistsForTwoEnvironments_CorrectValueIsReturned;
    [Test]
    procedure WhenFetchingASecret_EnvironmentNameIsTreatedCaseInsensitive;
    [Test]
    procedure WhenFetchingASecret_SecretNameIsTreatedCaseInsensitive;
    [Test]
    procedure WhenAddingASecret_SecretIsOverwritten;
    [Test]
    procedure WhenAddingASecret_SecretIsOverwrittenDifferentCase;
  end;

implementation

function TVaultTests.UniqueId: String;

begin
  Result := GUIDToString(TGuid.NewGuid);
end;

procedure TVaultTests.WhenAddingASecret_SecretIsOverwritten;
var
  Vault: IVault;
  Environment: String;
begin
  Environment := UniqueId;

  TVault.Add(Environment, 'Secret', 'Foo');
  TVault.Add(Environment, 'Secret', 'Bar');

  Vault := TVault.Create(Environment);

  Assert.AreEqual('Bar', Vault.GetSecret('Secret'), false);
end;

procedure TVaultTests.WhenAddingASecret_SecretIsOverwrittenDifferentCase;
var
  Vault: IVault;
  Environment: String;
begin
  Environment := UniqueId;

  TVault.Add(Environment, 'seCRET', 'Foo');
  TVault.Add(Environment, 'SECRet', 'Bar');

  Vault := TVault.Create(Environment);

  Assert.AreEqual('Bar', Vault.GetSecret('Secret'), false);
end;

procedure TVaultTests.WhenFetchingASecret_EnvironmentNameIsTreatedCaseInsensitive;
var
  Vault: IVault;
  Secret: String;
begin
  Secret := UniqueId;

  TVault.Add('EnViRoNmEnT', Secret, 'Foo');

  Vault := TVault.Create('ENVIronmenT');

  Assert.AreEqual('Foo', Vault.GetSecret(Secret), false);
end;

procedure TVaultTests.WhenFetchingASecret_IfItExistsForOtherEnvironmentOnly_EmptyValueIsReturned;
var
  Vault: IVault;
  ThisEnvironment: String;
  OtherEnvironment: String;
begin
  ThisEnvironment := UniqueId;
  OtherEnvironment := UniqueId;

  TVault.Add(OtherEnvironment, 'SECRET', 'Bar');

  Vault := TVault.Create(ThisEnvironment);

  Assert.IsEmpty(Vault.GetSecret('SECRET'));
end;

procedure TVaultTests.WhenFetchingASecret_IfItExistsForTwoEnvironments_CorrectValueIsReturned;
var
  Vault: IVault;
  ThisEnvironment: String;
  OtherEnvironment: String;
begin
  ThisEnvironment := UniqueId;
  OtherEnvironment := UniqueId;

  TVault.Add(ThisEnvironment, 'SECRET', 'Foo');
  TVault.Add(OtherEnvironment, 'SECRET', 'Bar');

  Vault := TVault.Create(ThisEnvironment);

  Assert.AreEqual('Foo', Vault.GetSecret('SECRET'), False);
end;

procedure TVaultTests.WhenFetchingASecret_SecretNameIsTreatedCaseInsensitive;
var
  Vault: IVault;
  Environment: String;
begin
  Environment := UniqueId;

  TVault.Add(Environment, 'seCREt', 'Foo');

  Vault := TVault.Create(Environment);

  Assert.AreEqual('Foo', Vault.GetSecret('SecReT'), false);
end;

initialization
  TDUnitX.RegisterTestFixture(TVaultTests);
end.

