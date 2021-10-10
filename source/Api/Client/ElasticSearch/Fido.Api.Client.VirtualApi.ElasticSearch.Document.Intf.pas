(*
 * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without Apiriction, including without limitation the rights
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

unit Fido.Api.Client.VirtualApi.ElasticSearch.Document.Intf;

interface

uses
  Rest.Types,
  System.Generics.Collections,
  System.Classes,

  Spring,
  Spring.Collections,

  Fido.OwningObject,
  Fido.Api.Client.VirtualApi.Intf,
  Fido.Api.Client.VirtualApi.Attributes,
  Fido.Api.Client.VirtualApi.Elasticsearch.Document.Dto.Request,
  Fido.Api.Client.VirtualApi.Elasticsearch.Document.Dto.Response;
  
type
  {$M+}
  IElasticsearchDocumentApi<Document: TElasticsearchDocumentRequest> = interface(IClientVirtualApi)
  ['{0B42421D-5E82-424E-A7B8-1886AB26283E}']

    [Endpoint(rmPost, '/{index}/{typename}')]
	  [RequestParam('Request')]
    function Add(const Index: string; const TypeName: string; const Request: Document): IElasticsearchDocumentResponse;
  end;

implementation

(**
  Example of Api registration:

interface

type
  IElasticsearchDocumentApiConfiguration = interface(IApiClientVirtualApiConfiguration)
    ['{A625B584-E1A1-4EC3-A9F1-47DBBC349551}']
  end;
  IElasticsearchDocumentApi_DocumentRequest = IElasticsearchDocumentApi<{{a descendant from TElasticsearchDocumentRequest}}>;
  TElasticsearchDocumentApiConfiguration = class(TApiClientVirtualApiConfiguration, IElasticsearchDocumentApiConfiguration);
...

implementation

...
  Container.RegisterType<IElasticsearchDocumentApiConfiguration>.DelegateTo(
    function: IElasticsearchDocumentApiConfiguration
    begin
      Result := TElasticsearchDocumentApiConfiguration.Create(
        {{Elasticsearch url and port}},
        {{Active flag}},
        {{Live environment Flag}})
    end).AsSingleton;

  Container.RegisterType<IElasticsearchDocumentApi_DocumentRequest, TJSONApiClientVirtualApi<IElasticsearchDocumentApi_DocumentRequest, IElasticsearchDocumentApiConfiguration>>;
...

**)

end.
