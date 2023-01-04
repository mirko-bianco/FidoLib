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

unit Fido.Http.ResponseInfo.Intf;

interface

uses
  System.Classes,

  Spring.Collections,

  Fido.Http.RequestInfo.Intf;

type
  TWSBytes = array of Byte;

  IHttpResponseInfo = interface(IInvokable)
    ['{251F6A9A-11DC-4487-8F3B-9389F41455A4}']

    procedure SetContentStream(const Stream: TStream);
    procedure SetResponseCode(const ResponseCode: Integer);
    function ResponseCode: Integer;
    procedure SetContentType(const ContentType: string);
    function EncodeString(const AString: string): string;
    procedure SetResponseText(const Text: string);
    procedure SetContentText(const Text: string);
    procedure SetCustomHeaders(const Headers: IDictionary<string, string>);
    function RawHeaders: TStrings;
  end;

implementation

end.
