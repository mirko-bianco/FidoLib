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
  public
    constructor Create(const RedisClient: IFidoRedisClient; const KeyPrefix: string);

    function Get(const Key: string): string;
    function Put(const Key: string; const Value: string): Boolean;
    function Delete(const Key: string): Boolean;
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

function TRedisKVStore.Delete(const Key: string): Boolean;
begin
  Result := Retries.Run<Boolean>(
    function: Boolean
    begin
      Result := FRedisClient.DEL(FormatKey(Key)) = 1;
    end);
end;

function TRedisKVStore.FormatKey(const Key: string): string;
begin
  Result := Format('%s%s', [FKeyPrefix, Key]);
end;

function TRedisKVStore.Get(const Key: string): string;
begin
  Result := Retries.Run<string>(
    function: string
    var
      Value: Nullable<string>;
    begin
      Result := '';
      Value := FRedisClient.GET(FormatKey(Key));
      if Value.HasValue then
        Result := Value.Value;
    end);
end;

function TRedisKVStore.Put(
  const Key: string;
  const Value: string): Boolean;
begin
  Result := Retries.Run<Boolean>(
    function: Boolean
    begin
      Result := FRedisClient.&SET(FormatKey(Key), Value);
    end);
end;

end.
