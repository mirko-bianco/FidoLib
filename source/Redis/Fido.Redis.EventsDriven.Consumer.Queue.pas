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

  Spring,

  Redis.Commons,
  Redis.Client,

  Fido.JSON.Marshalling,
  Fido.DesignPatterns.Retries,
  Fido.EventsDriven.Consumer.Queue.Intf,
  Fido.EventsDriven.Utils,

  Fido.Redis.Client.Intf;

type
  TRedisEventsDrivenQueueConsumer = class(TInterfacedObject, IEventsDrivenQueueConsumer)
  private
    FRedisClient: IFidoRedisClient;
  public
    constructor Create(const RedisClient: IFidoRedisClient);
    destructor Destroy; override;

    function Pop(const Key: string; var Payload: string): Boolean;
    procedure PushBack(const Key: string; const Payload: string);
  end;


implementation

{ TRedisEventsDrivenQueueConsumer }

constructor TRedisEventsDrivenQueueConsumer.Create(const RedisClient: IFidoRedisClient);
begin
  inherited Create;

  Guard.CheckNotNull(RedisClient, 'RedisClient');
  FRedisClient := RedisClient;
end;

destructor TRedisEventsDrivenQueueConsumer.Destroy;
begin
  FRedisClient := nil;
  inherited;
end;

function TRedisEventsDrivenQueueConsumer.Pop(const Key: string; var Payload: string): Boolean;
var
  EncodedValue: Nullable<string>;
begin
  EncodedValue := Retries.Run<Nullable<string>>(
    function: Nullable<string>
    begin
      Result := FRedisClient.RPOP(Key);
    end);

  Result := EncodedValue.HasValue;
  if Result then
    Payload := TNetEncoding.Base64.Decode(EncodedValue)
end;

procedure TRedisEventsDrivenQueueConsumer.PushBack(
  const Key: string;
  const Payload: string);
var
  EncodedValue: string;
begin
  EncodedValue := TNetEncoding.Base64.Encode(Payload);

  Retries.Run(
    procedure
    begin
      FRedisClient.LPUSH(Key, EncodedValue);
    end);
end;

end.
