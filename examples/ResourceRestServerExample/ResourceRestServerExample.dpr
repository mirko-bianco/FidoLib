program ResourceRestServerExample;

{$APPTYPE CONSOLE}

{$R *.res}

{$R 'webresources.res' 'webresources.rc'}

uses
  classes,
  System.SysUtils,
  System.IOUtils,
  Spring,
  Fido.Resource.StreamReader,
  Fido.Http.Types,
  Fido.Api.Server.Intf,
  Fido.Api.Server.Indy,
  Fido.Web.Server.Resources,
  TestData in 'TestData.pas';

var
  RestServer: IApiServer;
begin
  ReportMemoryLeaksOnShutdown := True;
  RestServer := TIndyApiServer.Create(
    8080,
    50,
    TSSLCertData.CreateEmpty);
  try
    try
      RestServer.RegisterResource(TTestResource.Create);
      RestServer.SetWebServer(TResourceWebServer.Create(TStreamResourceReader.Create));
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
