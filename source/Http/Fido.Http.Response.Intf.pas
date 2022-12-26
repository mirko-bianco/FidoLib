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

unit Fido.Http.Response.Intf;

interface

uses
  System.Classes,
  Spring.Collections,

  Fido.Http.Types,
  Fido.Http.ResponseInfo.Intf;

type
  IHttpResponse = interface(IInvokable)
    ['{39820464-7965-4614-BA5D-7AE4197F546C}']

    procedure SetResponseCode(const ResponseCode: Integer; const ResponseText: string = '');
    function Body: string;
    procedure SetBody(const Body: string);
    procedure SetStream(const Stream: TStream);
    function HeaderParams: IDictionary<string, string>;
    function MimeType: TMimeType;
    procedure SetMimeType(const MimeType: TMimeType);
    procedure ServeFile(const FilenamePath: string);
    procedure WriteHeader;
  end;

implementation

end.
