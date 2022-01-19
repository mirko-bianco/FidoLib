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

unit Fido.EventsDriven.Listener.Queue;

interface

uses
  System.TypInfo,
  System.Classes,
  System.Rtti,
  System.SysUtils,
  System.NetEncoding,
  System.Threading,
  System.Generics.Collections,

  Spring,
  Spring.Collections,

  Fido.Boxes,
  Fido.JSON.Marshalling,

  Fido.EventsDriven.Utils,
  Fido.EventsDriven.Listener.Intf,
  Fido.EventsDriven.Consumer.Queue.Intf;

type
  TEventsDrivenQueueListener = class (TInterfacedObject, IEventsDrivenListener)
  private var
    FActive: IBox<Boolean>;
    FLock: IReadWriteSync;
    FSubscriptions: IDictionary<string, TConsumerData>;
    FSubscriptionsTask: ITask;
    FPollingIntervalMSec: Integer;
  private
    procedure PerformEventPolling(const QueueConsumer: IEventsDrivenQueueConsumer);
  public
    constructor Create(const QueueConsumerFactoryFunc: TFunc<IEventsDrivenQueueConsumer>);
    destructor Destroy; override;

    procedure SubscribeTo(const Channel: string; const EventName: string; const ConsumerData: TConsumerData);
    procedure UnsubscribeFrom(const Channel: string; const EventName: string);

    procedure Stop;
  end;

implementation

{ TEventsDrivenQueueListener }

constructor TEventsDrivenQueueListener.Create(const QueueConsumerFactoryFunc: TFunc<IEventsDrivenQueueConsumer>);
begin
  inherited Create;

  Guard.CheckTrue(Assigned(QueueConsumerFactoryFunc), 'QueueConsumerFactoryFunc is not assigned');

  FSubscriptions := TCollections.CreateDictionary<string, TConsumerData>;
  FPollingIntervalMSec := 250;
  FLock := TMREWSync.Create;

  FActive := Box<Boolean>.Setup(True);

  FSubscriptionsTask := TTask.Run(
    procedure
    var
      Index: Integer;
      Steps: Integer;
      QueueConsumer: IEventsDrivenQueueConsumer;
    begin
      QueueConsumer := QueueConsumerFactoryFunc();
      Steps := FPollingIntervalMSec div 50;
      if Steps = 0 then
        Steps := 1;
      while Assigned(FActive) and FActive.Value do
      begin
        for Index := 1 to Steps do
          Sleep(10);
        PerformEventPolling(QueueConsumer);
      end;
      QueueConsumer := nil;
    end);
end;

procedure TEventsDrivenQueueListener.PerformEventPolling(const QueueConsumer: IEventsDrivenQueueConsumer);
var
  Items: TArray<TPair<string, TConsumerData>>;
begin
  if not Assigned(FLock) then
    Exit;

  FLock.BeginRead;
  try
    Items := FSubscriptions.ToArray;
  finally
    FLock.EndRead;
  end;

  if Length(Items) = 0 then
    Exit;

  TCollections.CreateList<TPair<string, TConsumerData>>(Items).ForEach(
    procedure(const Item: TPair<string, TConsumerData>)
    var
      Value: string;
      Ctx: TRttiContext;
      RttiType: TRttiType;
      LItem: TPair<string, TConsumerData>;
    begin
      LItem := Item;
      while Assigned(FActive) and FActive.Value and QueueConsumer.Pop(Item.Key, Value) do
      begin
        Ctx := TRttiContext.Create;
        RttiType := Ctx.GetType(Item.Value.Consumer.ClassType);

        TCollections.CreateList<TRttiMethod>(RttiType.GetMethods).
          Where(
            function(const Method: TRttiMethod): Boolean
            begin
              Result := Method.Name.Equals(LItem.Value.MethodName);
            end).
          ForEach(
            procedure(const Method: TRttiMethod)
            begin
              try
                Method.Invoke(
                  LItem.Value.Consumer,
                  [JSONUnmarshaller.To(Value, Method.GetParameters[0].ParamType.Handle)]);
              except
                QueueConsumer.PushBack(LItem.Key, Value);
              end;
            end);
      end;
    end);
end;

destructor TEventsDrivenQueueListener.Destroy;
begin
  FActive.UpdateValue(False);
  FSubscriptionsTask.Wait(FPollingIntervalMSec);
  inherited;
end;

procedure TEventsDrivenQueueListener.Stop;
begin
  FActive.UpdateValue(False);
end;

procedure TEventsDrivenQueueListener.SubscribeTo(
  const Channel: string;
  const EventName: string;
  const ConsumerData: TConsumerData);
begin
  FLock.BeginWrite;
  try
    FSubscriptions.Items[TEventsDrivenUtilities.FormatKey(Channel, EventName)] := ConsumerData;
  finally
    FLock.EndWrite;
  end;
end;

procedure TEventsDrivenQueueListener.UnsubscribeFrom(
  const Channel: string;
  const EventName: string);
begin
  FLock.BeginWrite;
  try
    FSubscriptions.Remove(TEventsDrivenUtilities.FormatKey(Channel, EventName));
  finally
    FLock.EndWrite;
  end;
end;

end.
