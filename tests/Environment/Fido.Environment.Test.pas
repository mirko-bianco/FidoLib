unit Fido.Environment.Test;

interface

uses
  DUnitX.TestFramework,
  SysUtils,

  Fido.Environment.Intf,
  Fido.Environment,
  Fido.Environment.Vault;

type
  [TestFixture]
  TEnvironmentTests = class
  private
    function UniqueId: String;
  public
    [Test]
    procedure UnknownEnvironmentAreTreatedAsDevelopment;
    [Test]
    procedure UnregisteredSecretIsEmpty;
    [Test]
    procedure GivenEnvironmentNameIsUsedInFetchingSecrets;
    [Test]
    procedure GivenCustomEnvironmentNameIsReturnedAsIs;
    [Test]
    procedure GivenStandardEnvironmentNameIsReturnedAsIs;
  end;

implementation

procedure TEnvironmentTests.GivenCustomEnvironmentNameIsReturnedAsIs;
var
  Environment: IEnvironment;
begin
  Environment := TEnvironment.Create('MyFooBarTestEnv');

  Assert.AreEqual('MyFooBarTestEnv', Environment.EnvironmentName);
end;

procedure TEnvironmentTests.GivenEnvironmentNameIsUsedInFetchingSecrets;
var
  EnvironmentName: String;
  SecretName: String;
  Value: String;
  Environment: IEnvironment;
begin
  EnvironmentName := UniqueId;
  SecretName := UniqueId;
  Value := UniqueId;

  Environment := TEnvironment.Create(EnvironmentName);
  TVault.Add('Foo', SecretName, UniqueId);
  TVault.Add(EnvironmentName, SecretName, Value);
  TVault.Add('Bar', SecretName, UniqueId);

  Assert.AreEqual(
    Value,
    Environment.GetSecret(SecretName)
  );
end;

procedure TEnvironmentTests.GivenStandardEnvironmentNameIsReturnedAsIs;
var
  Environment: IEnvironment;
begin
  Environment := TEnvironment.Create('proDUCTion');

  Assert.AreEqual(enProduction, Environment.EnvironmentType);
  Assert.AreEqual('proDUCTion', Environment.EnvironmentName, False);
end;

function TEnvironmentTests.UniqueId: String;

begin
  Result := GUIDToString(TGuid.NewGuid);
end;

procedure TEnvironmentTests.UnknownEnvironmentAreTreatedAsDevelopment;
var
  Environment: IEnvironment;
begin
  Environment := TEnvironment.Create(UniqueId);

  Assert.AreEqual(
    Environment.EnvironmentType,
    enDevelopment
  );
end;

procedure TEnvironmentTests.UnregisteredSecretIsEmpty;
var
  Environment: IEnvironment;
begin
  Environment := TEnvironment.Create(UniqueId);

  Assert.IsEmpty(Environment.GetSecret(UniqueId));
end;

initialization
  TDUnitX.RegisterTestFixture(TEnvironmentTests);
end.
