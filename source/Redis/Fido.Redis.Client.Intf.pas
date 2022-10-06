(*
 * Copyright 2022 Mirko Bianco (email: writetomirko@gmail.com)
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

unit Fido.Redis.Client.Intf;

interface

uses
  System.SysUtils,

  Spring,

  Redis.Commons,

  Fido.Functional;

type
  IFidoRedisClient = interface(IInvokable)
    ['{D3638B13-B487-4CC5-B1C5-8B52474F4990}']

    function DEL(const Keys: string; const Timeout: Cardinal = INFINITE): Context<Integer>;
    function GET(const Key: string; const Timeout: Cardinal = INFINITE): Context<Nullable<string>>;
    function &SET(const Key: string; const Value: string; const Timeout: Cardinal = INFINITE): Context<Boolean>;
    function RPOP(const Key: string; const Timeout: Cardinal = INFINITE): Context<Nullable<string>>;
    function LPUSH(const Key: string; const Value: string; const Timeout: Cardinal = INFINITE): Context<Integer>;
    function PUBLISH(const Key: string; const Value: string; const Timeout: Cardinal = INFINITE): Context<Integer>;
    function SUBSCRIBE(const Channel: string; aCallback: TProc<string, string>; aContinueOnTimeoutCallback: TRedisTimeoutCallback = nil; aAfterSubscribe: TProc = nil): Context<Void>;
  end;

  {$M+}
  IFidoRedisClientFactory = reference to function: IFidoRedisClient;
  {$M-}

implementation

end.
