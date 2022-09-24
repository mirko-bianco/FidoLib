program FidoLibTests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}
uses
  SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  Fido.Registration,
  Fido.JSON.Mapping,
  Fido.Environment.Test in 'Environment\Fido.Environment.Test.pas',
  Fido.Environment.Vault.Test in 'Environment\Fido.Environment.Vault.Test.pas',
  Tests.VirtualDTO.JSON in 'Json\Tests.VirtualDTO.JSON.pas',
  Tests.JSON.Marshalling in 'Json\Tests.JSON.Marshalling.pas',
  Fido.Api.Client.VirtualApi.Json.Base.Test in 'Api\Client\Fido.Api.Client.VirtualApi.Json.Base.Test.pas',
  Fido.Api.Client.VirtualApi.Json.CallApi.Test in 'Api\Client\Fido.Api.Client.VirtualApi.Json.CallApi.Test.pas',
  Fido.Api.Client.VirtualApi.Json.Test in 'Api\Client\Fido.Api.Client.VirtualApi.Json.Test.pas',
  Fido.Db.DatasetFieldAttributes.Test in 'Db\Fido.Db.DatasetFieldAttributes.Test.pas',
  Fido.Db.ListDataSet.DatasetFieldAttributes.Test in 'Db\Fido.Db.ListDataSet.DatasetFieldAttributes.Test.pas',
  Fido.Db.ListDataSet.Test in 'Db\Fido.Db.ListDataSet.Test.pas',
  Fido.Db.Transaction.Test in 'Db\Fido.Db.Transaction.Test.pas',
  Fido.Db.TransactionHandler.Base.Test in 'Db\Fido.Db.TransactionHandler.Base.Test.pas',
  Fido.Db.TransactionHandler.Test in 'Db\Fido.Db.TransactionHandler.Test.pas',
  Fido.OwningObject.Test in 'Fido.OwningObject.Test.pas',
  Fido.Immutable.Test in 'Fido.Immutable.Test.pas',
  Fido.Mappers.Test in 'Fido.Mappers.Test.pas',
  Fido.JWT.Manager.Test in 'JWT\Fido.JWT.Manager.Test.pas',
  Fido.DesignPatterns.Adapter.DataSetAsReadonlyList.Test in 'DesignPatterns\Fido.DesignPatterns.Adapter.DataSetAsReadonlyList.Test.pas',
  Fido.DesignPatterns.Adapter.JsonArrayAsReadonlyList.Test in 'DesignPatterns\Fido.DesignPatterns.Adapter.JsonArrayAsReadonlyList.Test.pas',
  Fido.DesignPatterns.Retries.Test in 'DesignPatterns\Fido.DesignPatterns.Retries.Test.pas',
  Fido.Boxes.Test in 'Fido.Boxes.Test.pas',
  Fido.Consul.KVStore.Test in 'Consul\Fido.Consul.KVStore.Test.pas',
  Fido.Consul.Service.Test in 'Consul\Fido.Consul.Service.Test.pas',
  Fido.EventsDriven.Listener.Queue.Test in 'EventsDriven\Fido.EventsDriven.Listener.Queue.Test.pas',
  Fido.EventsDriven.Publisher.Test in 'EventsDriven\Fido.EventsDriven.Publisher.Test.pas',
  Fido.EventsDriven.Subscriber.Test in 'EventsDriven\Fido.EventsDriven.Subscriber.Test.pas',
  Fido.EventsDriven.Listener.PubSub.Test in 'EventsDriven\Fido.EventsDriven.Listener.PubSub.Test.pas',
  Fido.Redis.KVStore.Test in 'Redis\Fido.Redis.KVStore.Test.pas',
  Fido.Redis.EventsDriven.Consumer.Queue.Test in 'Redis\Fido.Redis.EventsDriven.Consumer.Queue.Test.pas',
  Fido.Redis.EventsDriven.Producer.Queue.Test in 'Redis\Fido.Redis.EventsDriven.Producer.Queue.Test.pas',
  Fido.Redis.EventsDriven.Producer.PubSub.Test in 'Redis\Fido.Redis.EventsDriven.Producer.PubSub.Test.pas',
  Fido.Redis.EventsDriven.Consumer.PubSub.Test in 'Redis\Fido.Redis.EventsDriven.Consumer.PubSub.Test.pas',
  Fido.Redis.EventsDriven.Producer.QueueQueuePubSub.Test in 'Redis\Fido.Redis.EventsDriven.Producer.QueueQueuePubSub.Test.pas',
  Fido.Redis.EventsDriven.Consumer.QueuePubSub.Test in 'Redis\Fido.Redis.EventsDriven.Consumer.QueuePubSub.Test.pas',
  Fido.EventsDriven.Utils.Test in 'EventsDriven\Fido.EventsDriven.Utils.Test.pas',
  Fido.Memory.EventsDriven.Producer.PubSub.Test in 'EventsDriven\Fido.Memory.EventsDriven.Producer.PubSub.Test.pas',
  Fido.Memory.EventsDriven.Consumer.PubSub.Test in 'EventsDriven\Fido.Memory.EventsDriven.Consumer.PubSub.Test.pas',
  Fido.Memory.EventsDriven.Broker.PubSub.Test in 'EventsDriven\Fido.Memory.EventsDriven.Broker.PubSub.Test.pas',
  Fido.Utilities.Test in 'Fido.Utilities.Test.pas',
  Fido.Http.Utils.Test in 'Http\Fido.Http.Utils.Test.pas',
  Fido.Functional.Tests in 'Fido.Functional.Tests.pas',
  Fido.Web.Client.Websocket.Test in 'Web\Client\WebSocket\Fido.Web.Client.Websocket.Test.pas',
  Fido.Currying.Test in 'Fido.Currying.Test.pas',
  Fido.Caching.test in 'Fido.Caching.test.pas';

{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger : ITestLogger;
{$ENDIF}
begin
  ReportMemoryLeaksOnShutdown := True;
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //When true, Assertions must be made during tests;
    runner.FailsOnNoAsserts := False;

    //tell the runner how we will log things
    //Log to the console window if desired
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
    begin
      System.Writeln(E.ClassName, ': ', E.Message);
      {$IFNDEF CI}
      System.Readln;
      {$ENDIF}
    end;
  end;
{$ENDIF}
end.
