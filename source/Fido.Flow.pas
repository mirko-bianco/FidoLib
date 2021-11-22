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

 unit Fido.Flow;

interface

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.Rtti,

  Spring,
  Spring.Collections,

  Fido.DesignPatterns.Observer.Intf,
  Fido.DesignPatterns.Observable.Intf,
  Fido.DesignPatterns.Observer.Notification.Intf,
  Fido.Flow.Intf;

type
  EFlow = class(Exception);

  TFlow = class(TInterfacedObject, IFlow, IObserver)
  private type
    TInteraction = record
    private
      FSignalActor: Weak<IObservable>;
      FMessage: string;
      FSlot: Spring.TAction<TArray<TValue>>;
    public
      constructor Create(const SignalActor: IObservable; const Message: string; const Slot: Spring.TAction<TArray<TValue>>);

      function SignalActor: IObservable;
      function Message: string;
      function Slot: Spring.TAction<TArray<TValue>>;
    end;
  private
    FLock: IReadWriteSync;
    FObservables: ISet<Weak<IObservable>>;
    FInteractions: IList<TInteraction>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterInteraction(const SignalActor: IObservable; const Message: string; const SlotActor: TObject; const TypInfo: pTypeInfo; const MethodName: string;
      const MapParams: TFunc<TArray<TValue>, TArray<TValue>> = nil); overload;
    procedure RegisterInteraction(const SignalActor: IObservable; const Message: string; const SlotActor: IInterface; const TypInfo: pTypeInfo; const MethodName: string;
      const MapParams: TFunc<TArray<TValue>, TArray<TValue>> = nil); overload;
    procedure RegisterInteraction(const SignalActor: IObservable; const Message: string; const Slot: Spring.TAction<TArray<TValue>>); overload;

    procedure Unregister(const SignalActor: IObservable);

    // IObserver
    procedure Notify(const Sender: IInterface; const Notification: INotification);
  end;

implementation

{ TFlow }

constructor TFlow.Create;
begin
  inherited;

  FLock := TMREWSync.Create;
  FInteractions := TCollections.CreateList<TInteraction>;
  FObservables := TCollections.CreateSet<Weak<IObservable>>;
end;

destructor TFlow.Destroy;
begin
  FLock.BeginRead;
  try
    FObservables.ForEach(
      procedure(const Item: Weak<IObservable>)
      begin
        if not Item.IsAlive then
          Exit;
        Item.Target.UnregisterObserver(Self);
      end);
  finally
    FLock.EndRead;
  end;

  inherited;
end;

procedure TFlow.Notify(
  const Sender: IInterface;
  const Notification: INotification);
begin
  FLock.BeginRead;
  try
    FInteractions.Where(
      function(const Interaction: TInteraction): Boolean
      begin
        Result := (Interaction.SignalActor as IInterface = Sender) and
          (Notification.GetDescription = Interaction.FMessage);
      end).ForEach(
      procedure(const Interaction: TInteraction)
      begin
        TThread.Synchronize(
          nil,
          procedure
          begin
            Interaction.FSlot(Notification.GetData.AsType<TArray<TValue>>)
          end);
      end);
  finally
    FLock.EndRead;
  end;
end;

procedure TFlow.RegisterInteraction(
  const SignalActor: IObservable;
  const Message: string;
  const Slot: Spring.TAction<TArray<TValue>>);
begin
  if FObservables.Add(SignalActor) then
    SignalActor.RegisterObserver(Self);

  FLock.BeginWrite;
  try
    FInteractions.Add(TInteraction.Create(SignalActor, Message, Slot));

  finally
    FLock.EndWrite;
  end;
end;

procedure TFlow.Unregister(const SignalActor: IObservable);
var
  ToBeDeleted: IEnumerable<TInteraction>;
begin
  FLock.BeginWrite;
  try
    ToBeDeleted := FInteractions.Where(
      function(const Interaction: TInteraction): Boolean
      begin
        Result := Interaction.SignalActor = SignalActor;
      end);

    ToBeDeleted.ForEach(
      procedure(const Interaction: TInteraction)
      var
        Observable: IObservable;
      begin
        if not Supports(Interaction.SignalActor, IObservable, Observable) then
          raise EFlow.Create('SignalActor must be an IObservable');

        Observable.UnregisterObserver(Self);
      end);

    FInteractions.ExtractRange(ToBeDeleted.ToArray);
  finally
    FLock.EndWrite;
  end;

end;

procedure TFlow.RegisterInteraction(
  const SignalActor: IObservable;
  const Message: string;
  const SlotActor: TObject;
  const TypInfo: pTypeInfo;
  const MethodName: string;
  const MapParams: TFunc<TArray<TValue>, TArray<TValue>>);
var
  Proc: Spring.TAction<TArray<TValue>>;
  Method: TRttiMethod;
  Ctx: TRttiContext;
begin
  Method := Ctx.GetType(TypInfo).GetMethod(MethodName);

  if not Assigned(Method) then
    raise EFlow.CreateFmt('Method %s not found', [MethodName]);

  if Method.MethodKind <> TMethodKind.mkProcedure then
    raise EFlow.CreateFmt('Method %s is not a procedure', [MethodName]);

  Proc := procedure(const Values: TArray<TValue>)
    var
      MappedValues: TArray<TValue>;
    begin
      try
        if Assigned(MapParams) then
          MappedValues := MapParams(Values)
        else
          MappedValues := Values;

        Method.Invoke(SlotActor, MappedValues);
      except
        on E: Exception do
          raise EFlow.CreateFmt('Interaction failed. Error Message: %s', [E.Message]);
      end;
    end;

  RegisterInteraction(SignalActor, Message, Proc);
end;

procedure TFlow.RegisterInteraction(
  const SignalActor: IObservable;
  const Message: string;
  const SlotActor: IInterface;
  const TypInfo: pTypeInfo;
  const MethodName: string;
  const MapParams: TFunc<TArray<TValue>, TArray<TValue>>);
var
  Proc: Spring.TAction<TArray<TValue>>;
  Method: TRttiMethod;
  Ctx: TRttiContext;
  Value: TValue;
begin
  Method := Ctx.GetType(TypInfo).GetMethod(MethodName);

  if not Assigned(Method) then
    raise EFlow.CreateFmt('Method %s not found', [MethodName]);

  if Method.MethodKind <> TMethodKind.mkProcedure then
    raise EFlow.CreateFmt('Method %s is not a procedure', [MethodName]);

  Proc := procedure(const Values: TArray<TValue>)
    var
      MappedValues: TArray<TValue>;
    begin
      try
        if Assigned(MapParams) then
          MappedValues := MapParams(Values)
        else
          MappedValues := Values;

        Value := TValue.From<IInterface>(SlotActor);
        Method.Invoke(Value, MappedValues);
      except
        on E: Exception do
          raise EFlow.CreateFmt('Interaction failed. Error Message: %s', [E.Message]);
      end;
    end;

  RegisterInteraction(SignalActor, Message, Proc);
end;

{ TFlow.TInteraction }

constructor TFlow.TInteraction.Create(const SignalActor: IObservable; const Message: string; const Slot: Spring.TAction<TArray<TValue>>);
begin
  FSignalActor := SignalActor;
  FMessage := Message;
  FSlot := Slot;
end;

function TFlow.TInteraction.Message: string;
begin
  Result := FMessage;
end;

function TFlow.TInteraction.SignalActor: IObservable;
begin
  Result := FSignalActor;
end;

function TFlow.TInteraction.Slot: Spring.TAction<TArray<TValue>>;
begin
  Result := FSlot;
end;

end.

