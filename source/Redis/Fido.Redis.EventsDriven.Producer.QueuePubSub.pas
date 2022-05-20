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

function TRedisQueuePubSubEventsDrivenProducer.Push(
  const Key: string;
  const Payload: string;
  const Timeout: Cardinal): Context<Boolean>;
var
  EventId: string;
  Client: IFidoRedisClient;
  PushParams: TArray<string>;
  PublishParams: TArray<string>;
  PushFunc: Context<TArray<string>>.MonadFunc<Boolean>;
  PublishFunc: Context<TArray<string>>.MonadFunc<Boolean>;
begin
  Client := FRedisClient;
  EventId := TGuid.NewGuid.ToString;
  PushParams := [EventId, TNetEncoding.Base64.Encode(Payload)];
  PublishParams := [Key, EventId];

  PushFunc := function(const Params: TArray<string>): Context<Boolean>
    var
      DoLPushFunc: Context<TArray<string>>.MonadFunc<Integer>;
    begin
      DoLPushFunc := function(const Params: TArray<string>): Context<Integer>
      begin
        Result := Client.LPUSH(Params[0], Params[1], Timeout);
      end;

      Result := Retry<TArray<string>>.New(Params).Map<Integer>(DoLPushFunc, Retries.GetRetriesOnExceptionFunc()).Map<Boolean>(GreatherThan0);
    end;

  PublishFunc := function(const Params: TArray<string>): Context<Boolean>
    var
      DoPublishFunc: Context<TArray<string>>.MonadFunc<Integer>;
    begin
      DoPublishFunc := function(const Params: TArray<string>): Context<Integer>
        begin
          Result := Client.PUBLISH(Params[0], Params[1], Timeout);
        end;

      Result := Retry<TArray<string>>.New(Params).Map<Integer>(DoPublishFunc, Retries.GetRetriesOnExceptionFunc()).Map<Boolean>(GreatherThan0);
    end;

  Result := &If<TArray<string>>.New(PushParams).Map(PushFunc).&Then<Boolean>(
    &If<TArray<string>>.New(PublishParams).Map(PublishFunc).&Then<Boolean>(True, False),
    False);
end;

end.
