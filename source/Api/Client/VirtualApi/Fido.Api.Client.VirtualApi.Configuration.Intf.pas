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

unit Fido.Api.Client.VirtualApi.Configuration.Intf;

interface

uses
  Fido.Api.Client.VirtualApi.Call;

type
  IClientVirtualApiConfiguration = interface(IInvokable)
    ['{07ACEF78-09D9-4711-95BA-34CB44274DB5}']

    function BaseUrl: string;
    function Active: Boolean;
    function LiveEnvironment: Boolean;
  end;

  IActiveClientVirtualApiConfiguration = interface(IClientVirtualApiConfiguration)
    ['{B5A2A1A5-741B-48D2-8FEF-8A41461192D4}']

    procedure CallBegins(const Call: TClientVirtualApiCall);
    procedure CallEnded(const Call: TClientVirtualApiCall);
  end;

implementation

end.
