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

  Fido.JSON.Marshalling,
  Fido.DesignPatterns.Retries,
  Fido.EventsDriven.Publisher.Intf,
  Fido.EventsDriven.Producer.Intf,
  Fido.EventsDriven.Utils;

type
  TEventsDrivenPublisher = class(TInterfacedObject, IEventsDrivenPublisher)
  private var
    FProducerFactoryFunc: TFunc<IEventsDrivenProducer>;
  public
    constructor Create(const ProducerFactoryFunc: TFunc<IEventsDrivenProducer>);

    function Trigger(const Channel: string; const EventName: string; const Payload: string = ''): Boolean;
  end;

implementation

{ TEventsDrivenPublisher }

constructor TEventsDrivenPublisher.Create(const ProducerFactoryFunc: TFunc<IEventsDrivenProducer>);
begin
  inherited Create;

  Guard.CheckTrue(Assigned(ProducerFactoryFunc), 'ProducerFactoryFunc');
  FProducerFactoryFunc := ProducerFactoryFunc;
end;

function TEventsDrivenPublisher.Trigger(
  const Channel: string;
  const EventName: string;
  const Payload: string): Boolean;
var
  Producer: IEventsDrivenProducer;
begin
  Producer := FProducerFactoryFunc();

  Result := Retries.Run<Boolean>(
    function: Boolean
    begin
      Result := Producer.Push(TEventsDrivenUtilities.FormatKey(Channel, EventName), Payload);
    end);
end;

end.
