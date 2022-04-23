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

unit Fido.Memory.EventsDriven.Producer.PubSub;

interface

uses
  System.SysUtils,

  Fido.Utilities,
  Fido.EventsDriven.Producer.Intf,
  Fido.EventsDriven.Broker.PubSub.Intf;

type
  TMemoryPubSubEventsDrivenProducer<PayloadType> = class(TInterfacedObject, IEventsDrivenProducer<PayloadType>)
  private
    FBroker: IPubSubEventsDrivenBroker<PayloadType>;
  public
    constructor Create(const Broker: IPubSubEventsDrivenBroker<PayloadType>);

    function Push(const Key: string; const Payload: PayloadType): Boolean;
  end;

implementation

{ TMemoryPubSubEventsDrivenProducer<PayloadType> }

constructor TMemoryPubSubEventsDrivenProducer<PayloadType>.Create(const Broker: IPubSubEventsDrivenBroker<PayloadType>);
begin
  inherited Create;

  FBroker := Utilities.CheckNotNullAndSet(Broker, 'Broker');
end;

function TMemoryPubSubEventsDrivenProducer<PayloadType>.Push(
  const Key: string;
  const Payload: PayloadType): Boolean;
begin
  Result := FBroker.Push(Key, Payload);
end;

end.
