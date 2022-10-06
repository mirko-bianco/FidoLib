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

unit Fido.Redis.KVStore;

interface

uses
  System.SysUtils,
  Generics.Collections,

  Spring,

  Fido.Utilities,
  Fido.Functional,
  Fido.Functional.Retries,
  Fido.DesignPatterns.Retries,
  Fido.KVStore.Intf,

  Fido.Redis.Client.Intf;

type
  TRedisKVStore = class(TInterfacedObject, IKVStore)
  private var
    FKeyPrefix: string;
    FRedisClient: IFidoRedisClient;
  private
    function FormatKey(const Key: string): string;

    function Is1(const Value: Integer): Boolean;
    function DoDelete(const Timeout: Cardinal): Context<string>.MonadFunc<Integer>;
    function DoGet(const Timeout: Integer): Context<string>.FunctorFunc<string>;
    function GetValueOrDefault(const Value: Nullable<string>): string;
    function DoPut(const Timeout: Cardinal): Context<TArray<string>>.MonadFunc<Boolean>;
  public
    constructor Create(const RedisClient: IFidoRedisClient; const KeyPrefix: string);

    function Get(const Key: string; const Timeout: Cardinal = INFINITE): Context<string>;
    function Put(const Key: string; const Value: string; const Timeout: Cardinal = INFINITE): Context<Boolean>;
    function Delete(const Key: string; const Timeout: Cardinal = INFINITE): Context<Boolean>;
  end;

implementation

{ TRedisKVStore }

constructor TRedisKVStore.Create(
  const RedisClient: IFidoRedisClient;
  const KeyPrefix: string);
begin
  inherited Create;

  FKeyPrefix := KeyPrefix;
  FRedisClient := Utilities.CheckNotNullAndSet(RedisClient, 'RedisClient');
end;

function TRedisKVStore.DoDelete(const Timeout: Cardinal): Context<string>.MonadFunc<Integer>;
var
  Client: IFidoRedisClient;
begin
  Client := FRedisClient;
  Result := function(const Key: string): Context<Integer>
    begin
      Result := Client.DEL(Key, Timeout);
    end;
end;

function TRedisKVStore.Delete(
  const Key: string;
  const Timeout: Cardinal): Context<Boolean>;
begin
  Result := Retry<string>.
    New(Context<string>.
      New(Key).
      Map<string>(FormatKey)).
    Map<Integer>(DoDelete(Timeout), Retries.GetRetriesOnExceptionFunc()).
    Map<Boolean>(Is1);
end;

function TRedisKVStore.FormatKey(const Key: string): string;
begin
  Result := Format('%s%s', [FKeyPrefix, Key]);
end;

function TRedisKVStore.DoGet(const Timeout: Integer): Context<string>.FunctorFunc<string>;
var
  Client: IFidoRedisClient;
begin
  Client := FRedisClient;
  Result := function(const Key: string): string
  begin
    Result := Client.GET(Key, Timeout).Map<string>(GetValueOrDefault);
  end;
end;

function TRedisKVStore.GetValueOrDefault(const Value: Nullable<string>): string;
begin
  Result := Value.GetValueOrDefault;
end;

function TRedisKVStore.Get(
  const Key: string;
  const Timeout: Cardinal): Context<string>;
begin
  Result := Retry<string>.
    New(Context<string>.
      New(Key).
      Map<string>(FormatKey)).
    Map<string>(DoGet(Timeout), Retries.GetRetriesOnExceptionFunc());
end;

function TRedisKVStore.Is1(const Value: Integer): Boolean;
begin
  Result := Value = 1;
end;

function TRedisKVStore.DoPut(const Timeout: Cardinal): Context<TArray<string>>.MonadFunc<Boolean>;
var
  Client: IFidoRedisClient;
begin
  Client := FRedisClient;

  Result := function(const Params: TArray<string>): Context<Boolean>
    begin
      Result := Client.&SET(Params[0], Params[1], Timeout);
    end;
end;

function TRedisKVStore.Put(
  const Key: string;
  const Value: string;
  const Timeout: Cardinal): Context<Boolean>;
begin
  Result := Retry<TArray<string>>.
    New([FormatKey(Key), Value]).
    Map<Boolean>(DoPut(Timeout), Retries.GetRetriesOnExceptionFunc());
end;

end.
