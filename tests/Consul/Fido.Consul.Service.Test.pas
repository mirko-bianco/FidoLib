unit Fido.Consul.Service.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  DUnitX.TestFramework,

  Spring,
  Spring.Mocking,

  Fido.Exceptions,
  Fido.Testing.Mock.Utils,
  Fido.Consul.Types,
  Fido.Consul.Service.Intf,
  Fido.Consul.UseCases.Service.Register.Intf,
  Fido.Consul.UseCases.Service.DeRegister.Intf,
  Fido.Consul.Service;

type
  EConsulServiceTests = class(EFidoException);

  [TestFixture]
  TConsulServiceTests = class
  public
    [Test]
    procedure RegisterDoesNotRaiseAnyException;

    [Test]
    procedure RegisterRaisesEConsulServiceWhenRegisteringFails;

    [Test]
    procedure DeregisterDoesNotRaiseAnyException;

    [Test]
    procedure TestConsultHealthCheck;
  end;

implementation

procedure TConsulServiceTests.DeregisterDoesNotRaiseAnyException;
var
  Service: TConsulService;
  RegisterServiceUseCase: Mock<IConsulRegisterServiceUseCase>;
  DeregisterServiceUseCase: Mock<IConsulDeregisterServiceUseCase>;
  ServiceId: string;
  ServiceName: string;
  Address: string;
  Port: Integer;
  HealthEndPoint: string;
  HealthCheck: TConsulHealthCheck;
begin
  ServiceId := MockUtils.SomeString;
  ServiceName := Mockutils.SomeString;
  Address := MockUtils.SomeString;
  Port := 80;
  HealthEndPoint := MockUtils.SomeString;
  HealthCheck := TConsulHealthCheck.Create(Format('%s%s:%d%s', ['http://', Address, Port, HealthEndpoint]));

  RegisterServiceUseCase := Mock<IConsulRegisterServiceUseCase>.Create;
  RegisterServiceUseCase.Setup.Returns<string>(ServiceId).When.Run(
    ServiceName,
    Address,
    Port,
    HealthCheck);
  DeregisterServiceUseCase := Mock<IConsulDeregisterServiceUseCase>.Create;
  DeregisterServiceUseCase.Setup.Executes.When.Run(ServiceId);

  Service := TConsulService.Create(
    RegisterServiceUseCase,
    DeregisterServiceUseCase,
    function: TArray<string>
    begin
      Result := [Address];
    end);

  try
    Assert.WillNotRaiseAny(
      procedure
      begin
        Service.Register(ServiceName, Port, HealthEndPoint);
        Service.Deregister;
      end);
  finally
    Service.Free;
  end;

  RegisterServiceUseCase.Received(Times.Once).Run(ServiceName, Address, Port, HealthCheck);
  RegisterServiceUseCase.Received(Times.Never).Run(Arg.IsNotIn<string>([ServiceName]), Arg.IsNotIn<string>([Address]), Arg.IsNotIn<Integer>([Port]), Arg.IsNotIn<TConsulHealthCheck>([HealthCheck]),
    Arg.IsAny<string>);
  DeregisterServiceUseCase.Received(Times.Once).Run(ServiceId);
  DeregisterServiceUseCase.Received(Times.Never).Run(Arg.IsNotIn<string>(ServiceId));
end;

procedure TConsulServiceTests.RegisterDoesNotRaiseAnyException;
var
  Service: TConsulService;
  RegisterServiceUseCase: Mock<IConsulRegisterServiceUseCase>;
  DeregisterServiceUseCase: Mock<IConsulDeregisterServiceUseCase>;
  ServiceId: string;
  ServiceName: string;
  Address: string;
  Port: Integer;
  HealthEndPoint: string;
  HealthCheck: TConsulHealthCheck;
begin
  ServiceId := MockUtils.SomeString;
  ServiceName := Mockutils.SomeString;
  Address := MockUtils.SomeString;
  Port := 80;
  HealthEndPoint := MockUtils.SomeString;
  HealthCheck := TConsulHealthCheck.Create(Format('%s%s:%d%s', ['http://', Address, Port, HealthEndpoint]));

  RegisterServiceUseCase := Mock<IConsulRegisterServiceUseCase>.Create;
  RegisterServiceUseCase.Setup.Returns<string>(ServiceId).When.Run(
    ServiceName,
    Address,
    Port,
    HealthCheck);
  DeregisterServiceUseCase := Mock<IConsulDeregisterServiceUseCase>.Create;

  Service := TConsulService.Create(
    RegisterServiceUseCase,
    DeregisterServiceUseCase,
    function: TArray<string>
    begin
      Result := [Address];
    end);

  try
    Assert.WillNotRaiseAny(
      procedure
      begin
        Service.Register(ServiceName, Port, HealthEndPoint);
      end);
  finally
    Service.Free;
  end;

  RegisterServiceUseCase.Received(Times.Once).Run(ServiceName, Address, Port, HealthCheck);
  RegisterServiceUseCase.Received(Times.Never).Run(Arg.IsNotIn<string>([ServiceName]), Arg.IsNotIn<string>([Address]), Arg.IsNotIn<Integer>([Port]), Arg.IsNotIn<TConsulHealthCheck>([HealthCheck]),
    Arg.IsAny<string>);
  DeregisterServiceUseCase.Received(Times.Once).Run(Arg.IsAny<string>);
end;

procedure TConsulServiceTests.RegisterRaisesEConsulServiceWhenRegisteringFails;
var
  Service: TConsulService;
  RegisterServiceUseCase: Mock<IConsulRegisterServiceUseCase>;
  DeregisterServiceUseCase: Mock<IConsulDeregisterServiceUseCase>;
  ServiceId: string;
  ServiceName: string;
  Address: string;
  Port: Integer;
  HealthEndPoint: string;
  HealthCheck: TConsulHealthCheck;
begin
  ServiceId := MockUtils.SomeString;
  ServiceName := Mockutils.SomeString;
  Address := MockUtils.SomeString;
  Port := 443;
  HealthEndPoint := MockUtils.SomeString;
  HealthCheck := TConsulHealthCheck.Create(Format('%s%s:%d%s', ['https://', Address, Port, HealthEndpoint]));

  RegisterServiceUseCase := Mock<IConsulRegisterServiceUseCase>.Create;
  RegisterServiceUseCase.Setup.Raises<EConsulServiceTests>.When.Run(
    ServiceName,
    Address,
    Port,
    HealthCheck);
  DeregisterServiceUseCase := Mock<IConsulDeregisterServiceUseCase>.Create;

  Service := TConsulService.Create(
    RegisterServiceUseCase,
    DeregisterServiceUseCase,
    function: TArray<string>
    begin
      Result := [Address];
    end);

  try
    Assert.WillRaise(
      procedure
      begin
        Service.Register(ServiceName, Port, HealthEndPoint);
      end,
      EConsulService);
  finally
    Service.Free;
  end;

  RegisterServiceUseCase.Received(Times.Once).Run(ServiceName, Address, Port, HealthCheck);
  RegisterServiceUseCase.Received(Times.Never).Run(Arg.IsNotIn<string>([ServiceName]), Arg.IsNotIn<string>([Address]), Arg.IsNotIn<Integer>([Port]), Arg.IsNotIn<TConsulHealthCheck>([HealthCheck]),
    Arg.IsAny<string>);
  DeregisterServiceUseCase.Received(Times.Never).Run(Arg.IsAny<string>);
end;

procedure TConsulServiceTests.TestConsultHealthCheck;
var
  HealthCheck: TConsulHealthCheck;
  Address: string;
  Port: Integer;
  HealthEndPoint: string;
begin
  Address := MockUtils.SomeString;
  Port := 443;
  HealthEndPoint := MockUtils.SomeString;
  HealthCheck := TConsulHealthCheck.Create(Format('%s%s:%d%s', ['https://', Address, Port, HealthEndpoint]));

  Assert.AreEqual('90m', HealthCheck.DeregisterCriticalServiceAfter);
  Assert.AreEqual(Format('%s%s:%d%s', ['https://', Address, Port, HealthEndpoint]), HealthCheck.HTTP);
  Assert.AreEqual('10s', HealthCheck.Interval);
  Assert.AreEqual('1s', HealthCheck.Timeout);
end;

initialization
  TDUnitX.RegisterTestFixture(TConsulServiceTests);
end.
