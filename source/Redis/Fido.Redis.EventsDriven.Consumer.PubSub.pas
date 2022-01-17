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

unit Fido.Redis.EventsDriven.Consumer.PubSub;

interface

uses
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
  TRedisEventsDrivenPubSubConsumer = class(TInterfacedObject, IEventsDrivenPubSubConsumer)
  private
    FRedisClientFactoryFunc: TFunc<IFidoRedisClient>;
    FTasks: IDictionary<string, ITask>;
    FClosing: Boolean;
  public
    constructor Create(const RedisClientFactoryFunc: TFunc<IFidoRedisClient>);

    procedure Subscribe(const Channel: string; const EventName: string; OnNotify: TProc<string, string>);
    procedure Unsubscribe(const Channel: string; const EventName: string);

    procedure Stop;
  end;


implementation

{ TRedisEventsDrivenPubSubConsumer }

procedure TRedisEventsDrivenPubSubConsumer.Stop;
begin
  FClosing := True;
end;

constructor TRedisEventsDrivenPubSubConsumer.Create(const RedisClientFactoryFunc: TFunc<IFidoRedisClient>);
begin
  inherited Create;

  Guard.CheckNotNull(RedisClientFactoryFunc, 'RedisClientFactoryFunc');
  FRedisClientFactoryFunc := RedisClientFactoryFunc;

  FTasks := TCollections.CreateDictionary<string, ITask>;
  FClosing := False;
end;

procedure TRedisEventsDrivenPubSubConsumer.Subscribe(
  const Channel: string;
  const EventName: string;
  OnNotify: TProc<string, string>);
begin
  FTasks.Items[TEventsDrivenUtilities.FormatKey(Channel, EventName)] := TTask.Run(
    procedure
    begin
      FRedisClientFactoryFunc().SUBSCRIBE(
        TEventsDrivenUtilities.FormatKey(Channel, EventName),
        procedure(key: string; EncodedPayload: string)
        var
          DecodedPayload: string;
        begin
          DecodedPayload := TNetEncoding.Base64String.Decode(EncodedPayload);
          OnNotify(Key, DecodedPayload);
        end,
        function: Boolean
        begin
          Result := Assigned(Self) and (not FClosing);
        end);
    end);
end;

procedure TRedisEventsDrivenPubSubConsumer.Unsubscribe(
  const Channel: string;
  const EventName: string);
begin
  FTasks.Remove(TEventsDrivenUtilities.FormatKey(Channel, EventName));
end;

end.
