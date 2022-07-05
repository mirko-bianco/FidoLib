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
  Fido.Functional,
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
  MockProc: Context<Void>;
begin
  ServiceId := MockUtils.SomeString;
  ServiceName := Mockutils.SomeString;
  Address := MockUtils.SomeString;
  Port := 80;
  HealthEndPoint := MockUtils.SomeString;
  HealthCheck := TConsulHealthCheck.Create(Format('%s%s:%d%s', ['http://', Address, Port, HealthEndpoint]));

  RegisterServiceUseCase := Mock<IConsulRegisterServiceUseCase>.Create;
  RegisterServiceUseCase.Setup.Returns<Context<string>>(Context<string>.New(ServiceId)).When.Run(
    ServiceName,
    Address,
    Port,
    HealthCheck);
  DeregisterServiceUseCase := Mock<IConsulDeregisterServiceUseCase>.Create;
  MockProc := Context<Void>.New(Void.Get);
  DeregisterServiceUseCase.Setup.Returns<Context<Void>>(MockProc).When.Run(ServiceId);

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

  RegisterServiceUseCase.Received(Times.Once).Run(ServiceName, Address, Port, HealthCheck, '', INFINITE);
  RegisterServiceUseCase.Received(Times.Never).Run(Arg.IsNotIn<string>([ServiceName]), Arg.IsNotIn<string>([Address]), Arg.IsNotIn<Integer>([Port]), Arg.IsNotIn<TConsulHealthCheck>([HealthCheck]),
    Arg.IsAny<string>, Arg.IsAny<Cardinal>);
  DeregisterServiceUseCase.Received(Times.Once).Run(ServiceId, INFINITE);
  DeregisterServiceUseCase.Received(Times.Never).Run(Arg.IsNotIn<string>(ServiceId), Arg.IsNotIn<Cardinal>([INFINITE]));
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
  RegisterServiceUseCase.Setup.Returns<Context<string>>(Context<string>.New(ServiceId)).When.Run(
    ServiceName,
    Address,
    Port,
    HealthCheck);
  DeregisterServiceUseCase := Mock<IConsulDeregisterServiceUseCase>.Create;
  DeregisterServiceUseCase.Setup.Returns<Context<Void>>(Context<Void>.New(Void.Get)).When.Run(Arg.IsAny<string>, Arg.IsAny<Cardinal>);

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

  RegisterServiceUseCase.Received(Times.Once).Run(ServiceName, Address, Port, HealthCheck, '', INFINITE);
  RegisterServiceUseCase.Received(Times.Never).Run(Arg.IsNotIn<string>([ServiceName]), Arg.IsNotIn<string>([Address]), Arg.IsNotIn<Integer>([Port]), Arg.IsNotIn<TConsulHealthCheck>([HealthCheck]),
    Arg.IsAny<string>, Arg.IsNotIn<Cardinal>([INFINITE]));
  DeregisterServiceUseCase.Received(Times.Once).Run(Arg.IsAny<string>, Arg.IsAny<Cardinal>);
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
  RegisterServiceUseCase.Setup.Returns<Context<string>>(
    Context<string>.New(
      function: string
      begin
        raise EConsulServiceTests.Create('Error Message');
      end,
      INFINITE)).When.Run(
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

  RegisterServiceUseCase.Received(Times.Once).Run(ServiceName, Address, Port, HealthCheck, '', INFINITE);
  RegisterServiceUseCase.Received(Times.Never).Run(Arg.IsNotIn<string>([ServiceName]), Arg.IsNotIn<string>([Address]), Arg.IsNotIn<Integer>([Port]), Arg.IsNotIn<TConsulHealthCheck>([HealthCheck]),
    Arg.IsAny<string>, Arg.IsAny<Cardinal>);
  DeregisterServiceUseCase.Received(Times.Never).Run(Arg.IsAny<string>, Arg.IsAny<Cardinal>);
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
