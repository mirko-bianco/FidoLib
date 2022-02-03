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

unit Fido.Slots.Binding;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,

  Spring,
  Spring.Collections,

  Fido.Exceptions,
  Fido.DesignPatterns.Observable.Intf,
  Fido.Slots.Attributes,
  Fido.Slots.Intf;

type
  ESlotBinding = class(EFidoException);

  Slots = record
  private
    class function TryFindSignalActor(const SlotActor: TObject; const RttiType: TRttiType; const ObservableName: string; out SignalActor: IObservable): Boolean; static;
  public
    class procedure RegisterWithClass<T: class>(const TheSlots: ISlots; const SignalActor: IObservable; const Message: string; const SlotType: TSlotType; const SlotActor: T; const MethodName: string;
      const MapParams: TFunc<TArray<TValue>, TArray<TValue>> = nil); overload; static;

    class procedure Register(const TheSlots: ISlots; const SignalActor: IObservable; const SlotActor: TObject); overload; static;
  end;

implementation

{ Slots }

class function Slots.TryFindSignalActor(const SlotActor: TObject; const RttiType: TRttiType; const ObservableName: string; out SignalActor: IObservable): Boolean;
  var
    ObservableField: TRttiField;
    ObservableGetter: TRttiMethod;
    Value: TValue;
  begin
    Result := False;
    try
      ObservableField := RttiType.GetField(ObservableName);
      if Assigned(ObservableField) then
      begin
        if not ObservableField.GetValue(SlotActor).IsType<IObservable> then
          Exit(False);
        SignalActor := ObservableField.GetValue(SlotActor).AsType<IObservable>;
        Exit(True);
      end;

      ObservableGetter := RttiType.GetMethod(ObservableName);
      if Assigned(ObservableGetter) and
         (ObservableGetter.MethodKind = mkFunction) and
         (Length(ObservableGetter.GetParameters) = 0) then
      begin
        Value := ObservableGetter.Invoke(SlotActor, []);
        if not Value.IsType<IObservable> then
          Exit(False);

        SignalActor := Value.AsType<IObservable>;
        Exit(True);
      end;
    except
      on E: Exception do
        raise ESlotBinding.CreateFmt('Slots.Register raised an exception: %s', [E.Message]);
    end;
  end;

class procedure Slots.Register(
  const TheSlots: ISlots;
  const SignalActor: IObservable;
  const SlotActor: TObject);
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
begin
  Ctx := TRttiContext.Create;

  RttiType := Ctx.GetType(SlotActor.ClassType);

  TCollections.CreateList<TRttiMethod>(RttiType.GetMethods).
    ForEach(procedure(const Method: TRttiMethod)
    begin
      TCollections.CreateList<TCustomAttribute>(Method.GetAttributes).
        Where(function(const Attribute: TCustomAttribute): Boolean
          begin
            result := Attribute is SignalToSlotAttribute;
          end).
        ForEach(procedure(const Attribute: TCustomAttribute)
          var
            ObservableName: string;
            Message: string;
            SlotType: TSlotType;
            FoundSignalActor: IObservable;
          begin
            ObservableName := (Attribute as SignalToSlotAttribute).ObservableName;
            Message := (Attribute as SignalToSlotAttribute).Message;
            SlotType := (Attribute as SignalToSlotAttribute).SlotType;

            if not TryFindSignalActor(SlotActor, RttiType, ObservableName, FoundSignalActor) then
              raise ESlotBinding.CreateFmt('Slots.Register signal actor %s not found', [ObservableName]);

            if FoundSignalActor = SignalActor then
              TheSlots.Register(SignalActor, Message, SlotType, SlotActor, RttiType.Handle, Method.Name)
          end)
    end);
end;

class procedure Slots.RegisterWithClass<T>(
  const TheSlots: ISlots;
  const SignalActor: IObservable;
  const Message: string;
  const SlotType: TSlotType;
  const SlotActor: T;
  const MethodName: string;
  const MapParams: TFunc<TArray<TValue>, TArray<TValue>>);
begin
  TheSlots.Register(SignalActor, Message, SlotType, SlotActor, TypeInfo(T), MethodName, MapParams);
end;

end.
