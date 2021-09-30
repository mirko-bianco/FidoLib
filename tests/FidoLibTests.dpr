program FidoLibTests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}


uses
  SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  Fido.Registration,
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
  Fido.Mappers.Test in 'Fido.Mappers.Test.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
  ReportMemoryLeaksOnShutdown := True;
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    TDUnitX.Options.ExitBehavior := TDUnitXExitBehavior.Pause;

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
end.
