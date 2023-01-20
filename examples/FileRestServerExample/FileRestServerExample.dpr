program FileRestServerExample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  classes,
  System.SysUtils,
  System.IOUtils,
  Spring,
  Fido.Http.Types,
  Fido.Api.Server.Intf,
  Fido.Api.Server.Brook,
  Fido.Web.Server.Files,
  TestData in 'TestData.pas';

var
  RestServer: IApiServer;
begin
  ReportMemoryLeaksOnShutdown := True;
  RestServer := TBrookApiServer.Create(
    8080,
    0,
    False,
    50,
    mtJson,
    TSSLCertData.CreateEmpty);
  try
    try
      RestServer.SetWebServer(TFileWebServer.Create(
        'public',
        'index.html'));
      RestServer.RegisterResource(TTestResource.Create);
      RestServer.SetActive(True);

      Readln;
    except
      on E: Exception do
      begin
        Writeln(E.ClassName, ': ', E.Message);
        Readln;
      end;
    end;
  finally
    RestServer.SetActive(False);
    RestServer := nil;
  end;
end.
