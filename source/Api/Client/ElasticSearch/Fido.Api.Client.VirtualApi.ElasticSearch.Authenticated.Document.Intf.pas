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

unit Fido.Api.Client.VirtualApi.ElasticSearch.Authenticated.Document.Intf;

interface

uses
  Rest.Types,

  Fido.Api.Client.VirtualApi.Intf,
  Fido.Api.Client.VirtualApi.Attributes,
  Fido.Api.Client.VirtualApi.Elasticsearch.Document.Dto.Request,
  Fido.Api.Client.VirtualApi.Elasticsearch.Document.Dto.Response;

type
  {$M+}
  IElasticsearchDocumentAuthenticatedApi<Document: TElasticsearchDocumentRequest> = interface(IClientVirtualApi)
  ['{5D6A1A11-05BD-4D2E-9477-ABB30947516B}']

    [Endpoint(rmPost, '/{index}/{typename}')]
	  [RequestParam('Request')]
    [HeaderParam('Authorization')]
    function Add(const Authorization: string; const Index: string; const TypeName: string; const Request: Document): IElasticsearchDocumentResponse;
  end;

implementation

end.
