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

  Fido.Functional,
  Fido.Functional.Tries,
  Fido.Boxes,
  Fido.JSON.Marshalling,

  Fido.EventsDriven.Utils,
  Fido.EventsDriven.Listener.Intf,
  Fido.EventsDriven.Consumer.Queue.Intf;

type
  TQueueEventsDrivenListener<PayloadType> = class (TInterfacedObject, IEventsDrivenListener)
  private var
    FActive: IBox<Boolean>;
    FLock: IReadWriteSync;
    FSubscriptions: IDictionary<string, TConsumerData>;
    FSubscriptionsTask: ITask;
    FPollingIntervalMSec: Integer;
  private
    procedure PerformEventPolling(const QueueConsumer: IQueueEventsDrivenConsumer<PayloadType>);
  public
    constructor Create(const QueueConsumerFactoryFunc: TFunc<IQueueEventsDrivenConsumer<PayloadType>>);
    destructor Destroy; override;

    procedure SubscribeTo(const Channel: string; const EventName: string; const ConsumerData: TConsumerData);
    procedure UnsubscribeFrom(const Channel: string; const EventName: string);

    procedure Stop;
  end;

implementation

{ TQueueEventsDrivenListener<PayloadType> }

constructor TQueueEventsDrivenListener<PayloadType>.Create(const QueueConsumerFactoryFunc: TFunc<IQueueEventsDrivenConsumer<PayloadType>>);
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
      QueueConsumer: IQueueEventsDrivenConsumer<PayloadType>;
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

procedure TQueueEventsDrivenListener<PayloadType>.PerformEventPolling(const QueueConsumer: IQueueEventsDrivenConsumer<PayloadType>);
var
  Items: TArray<TPair<string, TConsumerData>>;
begin
  if not Assigned(FLock) then
    Exit;

  FLock.BeginRead;

  Items := &Try<Void>.New(Void.Get).Map<TArray<TPair<string, TConsumerData>>>(Void.MapFunc<TArray<TPair<string, TConsumerData>>>(function: TArray<TPair<string, TConsumerData>>
    begin
      Result := FSubscriptions.ToArray;
    end)).Match(procedure
    begin
      FLock.EndRead;
    end);

  if Length(Items) = 0 then
    Exit;

  TCollections.CreateList<TPair<string, TConsumerData>>(Items).ForEach(
    procedure(const Item: TPair<string, TConsumerData>)
    var
      Value: Nullable<PayloadType>;
      Ctx: TRttiContext;
      RttiType: TRttiType;
      LItem: TPair<string, TConsumerData>;
      CanContinue: Boolean;
    begin
      CanContinue := True;
      LItem := Item;
      while Assigned(FActive) and FActive.Value and CanContinue do
      begin
        Value := QueueConsumer.Pop(Item.Key);
        CanContinue := Value.HasValue;
        if not CanContinue then
          Break;

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
              TryOut<Void>.New(function: Void
                begin
                  Method.Invoke(LItem.Value.Consumer, TEventsDrivenUtilities.PayloadToMethodParams<PayloadType>(Value, Method));
                end).Match(function(const E: TObject): Void
                begin
                  QueueConsumer.PushBack(LItem.Key, Value).Value;
                end);
            end);
      end;
    end);
end;

destructor TQueueEventsDrivenListener<PayloadType>.Destroy;
begin
  FActive.UpdateValue(False);
  FSubscriptionsTask.Wait(FPollingIntervalMSec);
  inherited;
end;

procedure TQueueEventsDrivenListener<PayloadType>.Stop;
begin
  FActive.UpdateValue(False);
end;

procedure TQueueEventsDrivenListener<PayloadType>.SubscribeTo(
  const Channel: string;
  const EventName: string;
  const ConsumerData: TConsumerData);
begin
  FLock.BeginWrite;
  FSubscriptions.Items[TEventsDrivenUtilities.FormatKey(Channel, EventName)] := ConsumerData;
  FLock.EndWrite;
end;

procedure TQueueEventsDrivenListener<PayloadType>.UnsubscribeFrom(
  const Channel: string;
  const EventName: string);
begin
  FLock.BeginWrite;
  TryOut<Void>.New(function: Void
    begin
      FSubscriptions.Remove(TEventsDrivenUtilities.FormatKey(Channel, EventName));
    end).Match(procedure
    begin
      FLock.EndWrite;
    end);
end;

end.
