program ElasticSearchLogExample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,

  Spring,
  Spring.Container,
  Spring.Logging,
  Spring.Logging.Loggers,
  Spring.Logging.Controller,

  Fido.Logging.Appenders.ElasticSearch,
  Fido.Api.Client.VirtualApi.Json,
  Fido.Api.Client.VirtualApi.Configuration.Intf,
  Fido.Api.Client.VirtualApi.Configuration,
  Fido.Api.Client.VirtualApi.Elasticsearch.Document.Intf,
  Fido.Api.Client.VirtualApi.ElasticSearch.Document.Dto.Request
  ;

type
  IElasticsearchDocumentApiConfiguration = interface(IClientVirtualApiConfiguration)
    ['{A625B584-E1A1-4EC3-A9F1-47DBBC349551}']
  end;
  IElasticsearchDocumentApi_DocumentRequest = IElasticsearchDocumentApi<TElasticsearchDocumentRequest>;
  TElasticsearchDocumentApiConfiguration = class(TClientVirtualApiConfiguration, IElasticsearchDocumentApiConfiguration);

  TLogData = class
    Number: Extended;
    Date: TDateTime;
    Text: string;
  end;

var
  Container: Shared<TContainer>;
  Logger: ILogger;
  LogData: TLogData;
begin
  Container := TContainer.Create;

  Container.Value.RegisterType<IElasticsearchDocumentApiConfiguration>.DelegateTo(
    function: IElasticsearchDocumentApiConfiguration
    begin
      Result := TElasticsearchDocumentApiConfiguration.Create(
        'http://192.168.2.84:9200',
        true,
        true);
    end).AsSingleton;

  Container.Value.RegisterType<IElasticsearchDocumentApi_DocumentRequest, TJSONClientVirtualApi<IElasticsearchDocumentApi_DocumentRequest, IElasticsearchDocumentApiConfiguration>>;
  Container.Value.RegisterType<ILoggerController>.DelegateTo(
    function: ILoggerController
    begin
      Result := TLoggerController.Create;
      Result.AddAppender(
        TElasticsearchAppender.Create(
          Container.Value.Resolve<IElasticsearchDocumentApi_DocumentRequest>,
          'index_in_use',
          'typename_in_use'));
    end);
  Container.Value.RegisterType<ILogger, TLogger>;

  Container.Value.Build;

  Logger := Container.Value.Resolve<ILogger>;

  Logger.Log('This is a log');
  Logger.Fatal('This is a Fatal');

  LogData := TLogData.Create;
  LogData.Number := 42.42;
  LogData.Date := Now;
  LogData.Text := 'Some Random Text';

  Logger.Log(
    TLogEvent.Create(
      TLogLevel.Info,
      TLogEventType.Text,
      'This is a log with extra data',
      nil,
      LogData));

  try
    raise Exception.Create('Ouch!');
  except
    on E: Exception do
      Logger.Log('Some error happened.', E);
  end;

  Readln;
end.
