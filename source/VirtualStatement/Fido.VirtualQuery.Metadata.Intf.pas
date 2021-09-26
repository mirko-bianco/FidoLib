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

unit Fido.VirtualQuery.Metadata.Intf;

interface

uses
  Fido.VirtualQuery.Attributes;

type
{ IVirtualQueryMetadata contains query's metadata (used in testing/debugging)
  Data is provided by TVirtualQuery<T> class so it only takes cast e.g.

    if (VirtualQueryInstance as IVirtualQueryMetadata).Description then .. }

  IVirtualQueryMetadata = interface(IInvokable)
    ['{B4E2883B-690A-4681-A7E3-996A82028565}']
    function GetDescription: string;
    function GetSQLResource: string;

    property Description: string read GetDescription;
    property SQLResource: string read GetSQLResource;
  end;

implementation

end.
