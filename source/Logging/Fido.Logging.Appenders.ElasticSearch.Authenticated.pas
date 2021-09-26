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

unit Fido.Logging.Appenders.ElasticSearch.Authenticated;

interface

uses
  System.Classes,
  System.SysUtils,

  Spring,
  Spring.Logging,
  Spring.Logging.Appenders.Base,

  Fido.Api.Client.VirtualApi.ElasticSearch.Authenticated.Document.Intf,
  Fido.Api.Client.VirtualApi.Elasticsearch.Document.Dto.Request,
  Fido.Api.Client.VirtualApi.Elasticsearch.Document.Dto.Response;

type
  IElasticsearchAuthenticatedLoggingApi = IElasticsearchDocumentAuthenticatedApi<TElasticsearchDocumentRequest>;

  TElasticsearchAuthenticatedAppender = class(TLogAppenderBase)
  private
    FApi: IElasticsearchAuthenticatedLoggingApi;
    FIndex: string;
    FTypeName: string;
    FApiKey: string;
  protected
    procedure DoSend(const Event: TLogEvent); override;
  public
    constructor Create(
      const Api: IElasticsearchAuthenticatedLoggingApi;
      const Index: string;
      const TypeName: string;
      const ApiKey: string);
  end;

implementation

{ TElasticsearchAuthenticatedAppender }

constructor TElasticsearchAuthenticatedAppender.Create(
  const Api: IElasticsearchAuthenticatedLoggingApi; const Index, TypeName,
  ApiKey: string);
begin
  inherited Create;
  Guard.CheckNotNull(Api, 'Api');
  Guard.CheckTrue(not Index.IsEmpty, 'Index is empty');
  Guard.CheckTrue(not TypeName.IsEmpty, 'TypeName is empty');
  FApi := Api;
  FIndex := Index;
  FTypeName := TypeName;
  FApiKey := ApiKey;
end;

procedure TElasticsearchAuthenticatedAppender.DoSend(const Event: TLogEvent);
var
  ExceptionPtr: Pointer;
  LEvent: TLogEvent;
begin
  LEvent := Event;
  ExceptionPtr := AcquireExceptionObject;
  TThread.CreateAnonymousThread(
    procedure
    var
      Request: Shared<TElasticsearchDocumentRequest>;
      Response: IElasticsearchDocumentResponse;
      Content: TObject;
    begin
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

        Response := FApi.Add('ApiKey ' + FApiKey, FIndex, FTypeName, Request.Value);

    end).Start;
end;

end.
