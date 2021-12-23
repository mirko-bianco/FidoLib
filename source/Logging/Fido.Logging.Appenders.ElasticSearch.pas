(*
 * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

unit Fido.Logging.Appenders.ElasticSearch;

interface

uses
  System.Classes,
  System.SysUtils,

  Spring,
  Spring.Logging,
  Spring.Logging.Appenders.Base,

  Fido.Api.Client.VirtualApi.ElasticSearch.Document.Intf,
  Fido.Api.Client.VirtualApi.Elasticsearch.Document.Dto.Request,
  Fido.Api.Client.VirtualApi.Elasticsearch.Document.Dto.Response;

type
  IElasticsearchLoggingApi = IElasticsearchDocumentApi<TElasticsearchDocumentRequest>;

  TElasticsearchAppender = class(TLogAppenderBase)
  private
    FApi: IElasticsearchLoggingApi;
    FIndex: string;
    FTypeName: string;
  protected
    procedure DoSend(const Event: TLogEvent); override;
  public
    constructor Create(const Api: IElasticsearchLoggingApi; const Index: string; const TypeName: string);
  end;

implementation

constructor TElasticsearchAppender.Create(
  const Api: IElasticsearchLoggingApi;
  const Index: string;
  const TypeName: string);
begin
  inherited Create;
  Guard.CheckNotNull(Api, 'Api');
  Guard.CheckTrue(not Index.IsEmpty, 'Index is empty');
  Guard.CheckTrue(not TypeName.IsEmpty, 'TypeName is empty');
  FApi := Api;
  FIndex := Index;
  FTypeName := TypeName;
end;

procedure TElasticsearchAppender.DoSend(const Event: TLogEvent);
var
  ExceptionPtr: Pointer;
  LEvent: TLogEvent;
  Request: TElasticsearchDocumentRequest;
  Response: IElasticsearchDocumentResponse;
  Content: TObject;
begin
  LEvent := Event;
  ExceptionPtr := AcquireExceptionObject;

  Content := nil;
  if LEvent.Data.IsObject then
    Content := LEvent.Data.AsObject;
  Request := TElasticsearchDocumentRequest.Create(
    LEvent.Level,
    LEvent.EventType,
    LEvent.Msg,
    LEvent.TimeStamp,
    Exception(ExceptionPtr),
    Content);

  Response := FApi.Add(FIndex, FTypeName, Request);

  try
    Request.Free;
  except
  end;
end;

end.
