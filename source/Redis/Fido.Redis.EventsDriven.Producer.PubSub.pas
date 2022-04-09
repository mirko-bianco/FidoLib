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

unit Fido.Redis.EventsDriven.Producer.PubSub;

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

  Fido.Utilities,
  Fido.JSON.Marshalling,
  Fido.DesignPatterns.Retries,
  Fido.EventsDriven.Producer.Intf,
  Fido.EventsDriven.Utils,

  Fido.Redis.Client.Intf;

type
  TRedisPubSubEventsDrivenProducer = class(TInterfacedObject, IEventsDrivenProducer<string>)
  private var
    FRedisClient: IFidoRedisClient;
  public
    constructor Create(const RedisClient: IFidoRedisClient);

    function Push(const Key: string; const Payload: string): Boolean;
  end;

implementation

{ TRedisPubSubEventsDrivenProducer }

constructor TRedisPubSubEventsDrivenProducer.Create(const RedisClient: IFidoRedisClient);
begin
  inherited Create;

  FRedisClient := Utilities.CheckNotNullAndSet(RedisClient, 'RedisClient');
end;

function TRedisPubSubEventsDrivenProducer.Push(
  const Key: string;
  const Payload: string): Boolean;
var
  EncodedPayload: string;
begin
  EncodedPayload := TNetEncoding.Base64.Encode(Payload);
  Result := Retries.Run<Boolean>(
    function: Boolean
    begin
      Result := FRedisClient.PUBLISH(Key, EncodedPayload) > 0;
    end);
end;

end.
