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

unit Fido.Redis.EventsDriven.Producer.QueuePubSub;

interface

uses
  System.SysUtils,
  System.NetEncoding,
  System.Threading,
  System.Generics.Collections,

  Spring.Collections,

  Redis.Values,
  Redis.Commons,
  Redis.Client,

  Fido.Functional,
  Fido.Functional.Retries,
  Fido.Functional.Ifs,
  Fido.Utilities,
  Fido.JSON.Marshalling,
  Fido.DesignPatterns.Retries,
  Fido.EventsDriven.Producer.Intf,
  Fido.EventsDriven.Utils,

  Fido.Redis.Client.Intf;

type
  TRedisQueuePubSubEventsDrivenProducer = class(TInterfacedObject, IEventsDrivenProducer<string>)
  private var
    FRedisClient: IFidoRedisClient;

    function GreatherThan0(const Value: Integer): Boolean;
    function DoLPush(const Timeout: Cardinal): Context<TArray<string>>.MonadFunc<Integer>;
    function DoPublish(const Timeout: Cardinal): Context<TArray<string>>.MonadFunc<Integer>;
    function TryPush(const Timeout: Integer): Context<TArray<string>>.MonadFunc<Boolean>;
    function TryPublish(const Timeout: Integer): Context<TArray<string>>.MonadFunc<Boolean>;
  public
    constructor Create(const RedisClient: IFidoRedisClient);

    function Push(const Key: string; const Payload: string; const Timeout: Cardinal = INFINITE): Context<Boolean>;
  end;

implementation

{ TRedisQueuePubSubEventsDrivenProducer }

constructor TRedisQueuePubSubEventsDrivenProducer.Create(const RedisClient: IFidoRedisClient);
begin
  inherited Create;

  FRedisClient := Utilities.CheckNotNullAndSet(RedisClient, 'RedisClient');
end;

function TRedisQueuePubSubEventsDrivenProducer.GreatherThan0(const Value: Integer): Boolean;
begin
  Result := Value > 0;
end;

function TRedisQueuePubSubEventsDrivenProducer.DoLPush(const Timeout: Cardinal): Context<TArray<string>>.MonadFunc<Integer>;
var
  Client: IFidoRedisClient;
begin
  Client := FRedisClient;

  Result := function(const Params: TArray<string>): Context<Integer>
    begin
      Result := Client.LPUSH(Params[0], Params[1], Timeout);
    end;
end;

function TRedisQueuePubSubEventsDrivenProducer.DoPublish(const Timeout: Cardinal): Context<TArray<string>>.MonadFunc<Integer>;
var
  Client: IFidoRedisClient;
begin
  Client := FRedisClient;

  Result := function(const Params: TArray<string>): Context<Integer>
    begin
      Result := Client.PUBLISH(Params[0], Params[1], Timeout);
    end;
end;

function TRedisQueuePubSubEventsDrivenProducer.TryPush(const Timeout: Integer): Context<TArray<string>>.MonadFunc<Boolean>;
begin
  Result := function(const Params: TArray<string>): Context<Boolean>
    begin
      Result := Retry<TArray<string>>.New(Params).Map<Integer>(DoLPush(Timeout), Retries.GetRetriesOnExceptionFunc()).Map<Boolean>(GreatherThan0);
    end;
end;

function TRedisQueuePubSubEventsDrivenProducer.TryPublish(const Timeout: Integer): Context<TArray<string>>.MonadFunc<Boolean>;
begin
  Result := function(const Params: TArray<string>): Context<Boolean>
    begin
      Result := Retry<TArray<string>>.New(Params).Map<Integer>(DoPublish(Timeout), Retries.GetRetriesOnExceptionFunc()).Map<Boolean>(GreatherThan0);
    end;
end;

function TRedisQueuePubSubEventsDrivenProducer.Push(
  const Key: string;
  const Payload: string;
  const Timeout: Cardinal): Context<Boolean>;
var
  EventId: string;
begin
  EventId := TGuid.NewGuid.ToString;

//  Result := &If<Integer>.New(TryPush(EventId, TNetEncoding.Base64.Encode(Payload), Timeout)).Map(GreatherThan0).&Then<Boolean>(True, False);
//  Result := ThenElse.New(Result).&Then<Boolean>(Context<Integer>.New(TryPublish(Key, EventId, Timeout)).Map<Boolean>(GreatherThan0));

//  Result := &If<Integer>.New(TryPush(EventId, TNetEncoding.Base64.Encode(Payload), Timeout)).Map(GreatherThan0).&Then<Boolean>(True, False);
//      &If<Integer>.New(TryPublish(Key, EventId, Timeout)).Map(GreatherThan0).&Then<Boolean>(True, False), False);

  Result := &If<TArray<string>>.New([EventId, TNetEncoding.Base64.Encode(Payload)]).Map(TryPush(Timeout)).&Then<Boolean>(
    &If<TArray<string>>.New([Key, EventId]).Map(TryPublish(Timeout)).&Then<Boolean>(True, False),
    False);


//  Result := &If<Integer>.New(
//    Retry<TArray<string>>.New([EventId, TNetEncoding.Base64.Encode(Payload)]).Map<Integer>(DoLPush(Timeout), Retries.GetRetriesOnExceptionFunc())
//    ).Map(GreatherThan0).&Then<Boolean>(
//      &If<Integer>.New(
//        Retry<TArray<string>>.New([Key, EventId]).Map<Integer>(DoPublish(Timeout), Retries.GetRetriesOnExceptionFunc())
//    ).Map(GreatherThan0).&Then<Boolean>(True, False), False);
end;

end.
