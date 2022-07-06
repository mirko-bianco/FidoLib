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

unit Fido.Redis.EventsDriven.Consumer.Queue;

interface

uses
  System.SysUtils,
  System.NetEncoding,
  System.Variants,
  Generics.Collections,

  Spring,

  Redis.Commons,
  Redis.Client,

  Fido.Utilities,
  Fido.Functional,
  Fido.Functional.Retries,
  Fido.Functional.Ifs,
  Fido.JSON.Marshalling,
  Fido.DesignPatterns.Retries,
  Fido.EventsDriven.Consumer.Queue.Intf,
  Fido.EventsDriven.Utils,

  Fido.Redis.Client.Intf;

type
  TRedisQueueEventsDrivenConsumer = class(TInterfacedObject, IQueueEventsDrivenConsumer<string>)
  private
    FRedisClient: IFidoRedisClient;
    function DoRPop(const Timeout: Cardinal): Context<string>.MonadFunc<Nullable<string>>;
    function HasValue(const Value: Nullable<string>): Boolean;
    function DecodeValue(const Value: Nullable<string>): Nullable<string>;
    function DoLPush(const Timeout: Cardinal): Context<TArray<string>>.MonadFunc<Integer>;
    function CheckLPushResult(const Value: Integer): Context<Boolean>;

  public
    constructor Create(const RedisClient: IFidoRedisClient);

    function Pop(const Key: string; const Timeout: Cardinal = INFINITE): Context<Nullable<string>>;
    function PushBack(const Key: string; const Payload: string; const Timeout: Cardinal = INFINITE): Context<Boolean>;
  end;

implementation

{ TRedisQueueEventsDrivenConsumer }

constructor TRedisQueueEventsDrivenConsumer.Create(const RedisClient: IFidoRedisClient);
begin
  inherited Create;

  FRedisClient := Utilities.CheckNotNullAndSet(RedisClient, 'RedisClient');
end;

function TRedisQueueEventsDrivenConsumer.DoRPop(const Timeout: Cardinal): Context<string>.MonadFunc<Nullable<string>>;
var
  Client: IFidoRedisClient;
begin
  Client := FRedisClient;

  Result := function(const Key: string): Context<Nullable<string>>
    begin
      Result := FRedisClient.RPOP(Key, Timeout);
    end;
end;

function TRedisQueueEventsDrivenConsumer.HasValue(const Value: Nullable<string>): Boolean;
begin
  Result := Value.HasValue;
end;

function TRedisQueueEventsDrivenConsumer.DecodeValue(const Value: Nullable<string>): Nullable<string>;
begin
  if Value.HasValue then
    Result := TNetEncoding.Base64.Decode(Value);
end;

function TRedisQueueEventsDrivenConsumer.Pop(
  const Key: string;
  const Timeout: Cardinal): Context<Nullable<string>>;
var
  NullValue: Nullable<string>;
begin
  Result := &If<Nullable<string>>.New(Retry<string>.New(Key).Map<Nullable<string>>(DoRPop(Timeout), Retries.GetRetriesOnExceptionFunc())).Map(HasValue).&Then<Nullable<string>>(DecodeValue, NullValue);
end;

function TRedisQueueEventsDrivenConsumer.DoLPush(const Timeout: Cardinal): Context<TArray<string>>.MonadFunc<Integer>;
var
  Client: IFidoRedisClient;
begin
  Client := FRedisClient;

  Result := function(const Params: TArray<string>): Context<Integer>
    begin
      Result := Client.LPUSH(Params[0], Params[1], Timeout);
    end
end;

function TRedisQueueEventsDrivenConsumer.CheckLPushResult(const Value: Integer): Context<Boolean>;
begin
  Result := Value > 0;
end;

function TRedisQueueEventsDrivenConsumer.PushBack(
  const Key: string;
  const Payload: string;
  const Timeout: Cardinal): Context<Boolean>;
begin
  Result := Retry<TArray<string>>.New([Key, TNetEncoding.Base64.Encode(Payload)]).Map<Integer>(DoLPush(Timeout), Retries.GetRetriesOnExceptionFunc()).Map<Boolean>(CheckLPushResult);
end;

end.
