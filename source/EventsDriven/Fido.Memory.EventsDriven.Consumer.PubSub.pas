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

unit Fido.Memory.EventsDriven.Consumer.PubSub;

interface

uses
  System.Classes,
  System.SysUtils,

  Fido.Utilities,
  Fido.EventsDriven.Consumer.PubSub.Intf,
  Fido.EventsDriven.Utils,
  Fido.EventsDriven.Broker.PubSub.Intf;

type
  TMemoryPubSubEventsDrivenConsumer<PayloadType> = class(TInterfacedObject, IPubSubEventsDrivenConsumer<PayloadType>)
  private
    FBroker: IPubSubEventsDrivenBroker<PayloadType>;
  public
    constructor Create(const Broker: IPubSubEventsDrivenBroker<PayloadType>);

    procedure Subscribe(const Channel: string; const EventName: string; const OnNotify: TProc<string, PayloadType>);
    procedure Unsubscribe(const Channel: string; const EventName: string);

    procedure Stop;
  end;

implementation

{ TMemoryPubSubEventsDrivenConsumer<PayloadType> }

procedure TMemoryPubSubEventsDrivenConsumer<PayloadType>.Stop;
begin
  FBroker.Stop(Self);
end;

constructor TMemoryPubSubEventsDrivenConsumer<PayloadType>.Create(const Broker: IPubSubEventsDrivenBroker<PayloadType>);
begin
  inherited Create;

  FBroker := Utilities.CheckNotNullAndSet(Broker, 'Broker');
end;

procedure TMemoryPubSubEventsDrivenConsumer<PayloadType>.Subscribe(
  const Channel: string;
  const EventName: string;
  const OnNotify: TProc<string, PayloadType>);
begin
  FBroker.Subscribe(Self, TEventsDrivenUtilities.FormatKey(Channel, EventName), OnNotify);
end;

procedure TMemoryPubSubEventsDrivenConsumer<PayloadType>.Unsubscribe(
  const Channel: string;
  const EventName: string);
begin
  FBroker.Unsubscribe(Self, TEventsDrivenUtilities.FormatKey(Channel, EventName));
end;

end.
