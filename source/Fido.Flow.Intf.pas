(*
 * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
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

 unit Fido.Flow.Intf;

interface

uses
  System.SysUtils,

  Spring,

  Fido.DesignPatterns.Observer.Intf,
  Fido.DesignPatterns.Observable.Intf;

type
  IFlow = interface(IObserver)
    ['{72785413-37E9-41EE-B3BA-FCBFDFE8BFFF}']

    procedure RegisterInteraction(const SignalActor: IObservable; const Message: string; const SlotActor: TObject; const TypInfo: pTypeInfo; const MethodName: string;
      const MapParams: TFunc<TArray<TValue>, TArray<TValue>> = nil); overload;
    procedure RegisterInteraction(const SignalActor: IObservable; const Message: string; const SlotActor: IInterface; const TypInfo: pTypeInfo; const MethodName: string;
      const MapParams: TFunc<TArray<TValue>, TArray<TValue>> = nil); overload;
    procedure RegisterInteraction(const SignalActor: IObservable; const Message: string; const Slot: Spring.TAction<TArray<TValue>>); overload;

    procedure Unregister(const SignalActor: IObservable);
  end;

  Flows = record
    class procedure RegisterInteractionWithClass<T: class>(const Flow: IFlow; const SignalActor: IObservable; const Message: string; const SlotActor: T; const MethodName: string;
      const MapParams: TFunc<TArray<TValue>, TArray<TValue>> = nil); overload; static;
    class procedure RegisterInteractionWithInterface<T: IInterface>(const Flow: IFlow; const SignalActor: IObservable; const Message: string; const SlotActor: T; const MethodName: string;
      const MapParams: TFunc<TArray<TValue>, TArray<TValue>> = nil); overload; static;
  end;

implementation

{ Flows }

class procedure Flows.RegisterInteractionWithClass<T>(
  const Flow: IFlow;
  const SignalActor: IObservable;
  const Message: string;
  const SlotActor: T;
  const MethodName: string;
  const MapParams: TFunc<TArray<TValue>, TArray<TValue>>);
begin
  Flow.RegisterInteraction(SignalActor, Message, SlotActor, TypeInfo(T), MethodName, MapParams);
end;

class procedure Flows.RegisterInteractionWithInterface<T>(
  const Flow: IFlow;
  const SignalActor: IObservable;
  const Message: string;
  const SlotActor: T;
  const MethodName: string;
  const MapParams: TFunc<TArray<TValue>, TArray<TValue>>);
begin
  Flow.RegisterInteraction(SignalActor, Message, SlotActor, TypeInfo(T), MethodName, MapParams);
end;

end.
