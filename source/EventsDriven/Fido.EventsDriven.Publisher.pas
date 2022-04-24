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

unit Fido.EventsDriven.Publisher;

interface

uses
  System.SysUtils,
  System.Threading,
  System.Generics.Collections,

  Spring,
  Spring.Collections,

  Fido.Utilities,
  Fido.Functional,
  Fido.Functional.Retries,
  Fido.JSON.Marshalling,
  Fido.DesignPatterns.Retries,
  Fido.EventsDriven.Publisher.Intf,
  Fido.EventsDriven.Producer.Intf,
  Fido.EventsDriven.Utils;

type
  TEventsDrivenPublisher<PayloadType> = class(TInterfacedObject, IEventsDrivenPublisher<PayloadType>)
  private
    FProducerFactoryFunc: TFunc<IEventsDrivenProducer<PayloadType>>;
    function DoPush: Context<TArray<TValue>>.MonadFunc<Boolean>;

  public
    constructor Create(const ProducerFactoryFunc: TFunc<IEventsDrivenProducer<PayloadType>>);

    function Trigger(const Channel: string; const EventName: string; const Payload: PayloadType): Context<Boolean>;
  end;

implementation

{ TEventsDrivenPublisher<PayloadType> }

constructor TEventsDrivenPublisher<PayloadType>.Create(const ProducerFactoryFunc: TFunc<IEventsDrivenProducer<PayloadType>>);
begin
  inherited Create;

  FProducerFactoryFunc := Utilities.CheckNotNullAndSet<TFunc<IEventsDrivenProducer<PayloadType>>>(ProducerFactoryFunc, 'ProducerFactoryFunc');
end;

function TEventsDrivenPublisher<PayloadType>.DoPush: Context<TArray<TValue>>.MonadFunc<Boolean>;
var
  FactoryFunc: TFunc<IEventsDrivenProducer<PayloadType>>;
begin
  FactoryFunc := FProducerFactoryFunc;

  Result := function(const PushData: TArray<TValue>): Context<Boolean>
    begin
      Result := FactoryFunc().Push(PushData[0].AsType<string>, PushData[1].AsType<PayloadType>);
    end;
end;

function TEventsDrivenPublisher<PayloadType>.Trigger(
  const Channel: string;
  const EventName: string;
  const Payload: PayloadType): Context<Boolean>;
begin
  Result := Retry<TArray<TValue>>.New([TEventsDrivenUtilities.FormatKey(Channel, EventName), TValue.From<PayloadType>(Payload)]).Map<Boolean>(DoPush(), Retries.GetRetriesOnExceptionFunc());
end;

end.
