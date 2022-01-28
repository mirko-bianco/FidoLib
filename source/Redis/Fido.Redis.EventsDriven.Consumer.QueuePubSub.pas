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

unit Fido.Redis.EventsDriven.Consumer.QueuePubSub;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.NetEncoding,

  Spring,
  Spring.Collections,

  Redis.Commons,
  Redis.Command,
  Redis.Client,

  Fido.JSON.Marshalling,
  Fido.DesignPatterns.Retries,
  Fido.EventsDriven.Consumer.PubSub.Intf,
  Fido.EventsDriven.Utils,

  Fido.Redis.Client.Intf;

type
  TRedisQueuePubSubEventsDrivenConsumer = class(TInterfacedObject, IPubSubEventsDrivenConsumer<string>)
  private
    FLock: IReadWriteSync;
    FRedisClientFactoryFunc: TFunc<IFidoRedisClient>;
    FTasks: IDictionary<string, ITask>;
    FClosing: Boolean;

    procedure TryPop(const RedisClient: IFidoRedisClient; const Key: string; const OnNotify: TProc<string, string>);
  public
    constructor Create(const RedisClientFactoryFunc: TFunc<IFidoRedisClient>);

    procedure Subscribe(const Channel: string; const EventName: string; const OnNotify: TProc<string, string>);
    procedure Unsubscribe(const Channel: string; const EventName: string);

    procedure Stop;
  end;

implementation

{ TRedisQueuePubSubEventsDrivenConsumer }

procedure TRedisQueuePubSubEventsDrivenConsumer.TryPop(
  const RedisClient: IFidoRedisClient;
  const Key: string;
  const OnNotify: TProc<string, string>);
var
  EncodedValue: Nullable<string>;
  Payload: string;
begin
  EncodedValue := '';

  // Skip channel opening message
  if Key.Equals(TGuid.Empty.ToString) then
    Exit;

  EncodedValue := Retries.Run<Nullable<string>>(
    function: Nullable<string>
    begin
      Result := RedisClient.RPOP(Key);
    end);

  if not EncodedValue.HasValue then
    Exit;

  Payload := TNetEncoding.Base64.Decode(EncodedValue);
  OnNotify(Key, Payload);
end;

procedure TRedisQueuePubSubEventsDrivenConsumer.Stop;
begin
  FClosing := True;
end;

constructor TRedisQueuePubSubEventsDrivenConsumer.Create(const RedisClientFactoryFunc: TFunc<IFidoRedisClient>);
begin
  inherited Create;

  Guard.CheckNotNull(RedisClientFactoryFunc, 'RedisClientFactoryFunc');
  FRedisClientFactoryFunc := RedisClientFactoryFunc;

  FLock := TMREWSync.Create;
  FTasks := TCollections.CreateDictionary<string, ITask>;
  FClosing := False;
end;

procedure TRedisQueuePubSubEventsDrivenConsumer.Subscribe(
  const Channel: string;
  const EventName: string;
  const OnNotify: TProc<string, string>);
var
  Key: string;
begin
  Key := TEventsDrivenUtilities.FormatKey(Channel, EventName);
  FTasks.Items[Key] := TTask.Run(
    procedure
    begin
      FRedisClientFactoryFunc().SUBSCRIBE(
        Key,
        procedure(key: string; QueueKey: string)
        begin
          TryPop(FRedisClientFactoryFunc(), QueueKey, OnNotify);
        end,
        function: Boolean
        begin
          Result := Assigned(Self) and (not FClosing);
        end);
    end);
end;

procedure TRedisQueuePubSubEventsDrivenConsumer.Unsubscribe(const Channel, EventName: string);
var
  Key: string;
begin
  Key := TEventsDrivenUtilities.FormatKey(Channel, EventName);
  FTasks.Items[Key] := nil;
  FTasks.Remove(Key);
end;

end.
