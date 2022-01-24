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

unit Fido.EventsDriven.Listener.PubSub;

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
  Fido.EventsDriven.Consumer.PubSub.Intf;

type
  TPubSubEventsDrivenListener = class (TInterfacedObject, IEventsDrivenListener)
  private var
    FPubSubConsumer: IPubSubEventsDrivenConsumer;
  public
    constructor Create(const PubSubConsumer: IPubSubEventsDrivenConsumer);

    procedure SubscribeTo(const Channel: string; const EventName: string; const ConsumerData: TConsumerData);
    procedure UnsubscribeFrom(const Channel: string; const EventName: string);

    procedure Stop;
  end;

implementation

{ TPubSubEventsDrivenListener }

constructor TPubSubEventsDrivenListener.Create(const PubSubConsumer: IPubSubEventsDrivenConsumer);
begin
  inherited Create;

  Guard.CheckNotNull(PubSubConsumer, 'PubSubConsumer');
  FPubSubConsumer := PubSubConsumer;
end;

procedure TPubSubEventsDrivenListener.Stop;
begin
  FPubSubConsumer.Stop;
end;

procedure TPubSubEventsDrivenListener.SubscribeTo(
  const Channel: string;
  const EventName: string;
  const ConsumerData: TConsumerData);
var
  LConsumerData: TConsumerData;
begin
  LConsumerData := ConsumerData;
  FPubSubConsumer.Subscribe(
    Channel,
    EventName,
    procedure(Key: string; Payload: string)
    var
      Ctx: TRttiContext;
      RttiType: TRttiType;
    begin
      Ctx := TRttiContext.Create;
      RttiType := Ctx.GetType(LConsumerData.Consumer.ClassType);

      TCollections.CreateList<TRttiMethod>(RttiType.GetMethods).
        Where(
          function(const Method: TRttiMethod): Boolean
          begin
            Result := Method.Name.Equals(LConsumerData.MethodName);
          end).
        ForEach(
          procedure(const Method: TRttiMethod)
          begin
            Method.Invoke(LConsumerData.Consumer, TEventsDrivenUtilities.PayloadToMethodParams(Payload, Method));
          end);
    end);
end;

procedure TPubSubEventsDrivenListener.UnsubscribeFrom(
  const Channel: string;
  const EventName: string);
begin
  FPubSubConsumer.Unsubscribe(Channel, EventName);
end;

end.
