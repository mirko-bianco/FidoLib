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

unit Fido.Api.Client.VirtualApi.Elasticsearch.Document.Dto.Response;

interface

type
  IShards = interface(IInvokable)
    ['{508BA920-FB17-4B30-B061-7EA424D0B96D}']

    function Total: Integer;
    function Failed: Integer;
    function Successful: Integer;
  end;

  IElasticsearchDocumentResponse = interface(IInvokable)
    ['{7A6B274C-68F9-4CD8-A73A-4CDA603ABEB0}']

    function Shards: IShards;
    function _Index: string;
    function _Type: string;
    function _Id: Integer;
    function _Version: Integer;
    function _Seq_No: Integer;
    function _Primary_Term: Integer;
    function Result: string;
  end;

implementation

end.
