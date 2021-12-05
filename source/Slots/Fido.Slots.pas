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

 unit Fido.Slots;

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
  Fido.Slots.Intf;

type
  ESlots = class(Exception);

  TSlots = class(TInterfacedObject, ISlots, IObserver)
  private type
    TSignalSlot = record
    private
      FSignalActor: Weak<IObservable>;
      FMessage: string;
      FSlot: Spring.TAction<TArray<TValue>>;
      FSlotType: TSlotType;

      function GetSignalActor: IObservable;
    public
      constructor Create(const SignalActor: IObservable; const Message: string; const SlotType: TSlotType; const Slot: Spring.TAction<TArray<TValue>>);

      property SignalActor: IObservable read GetSignalActor;
      property Message: string read FMessage;
      property SlotType: TSlotType read FSlotType;
      property Slot: Spring.TAction<TArray<TValue>> read FSlot;
    end;
  private
    FLock: IReadWriteSync;
    FObservables: ISet<Weak<IObservable>>;
    FSignalSlots: IList<TSignalSlot>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Register(const SignalActor: IObservable; const Message: string; const SlotType: TSlotType; const SlotActor: TObject; const TypInfo: pTypeInfo; const MethodName: string;
      const MapParams: TFunc<TArray<TValue>, TArray<TValue>> = nil); overload;
    procedure Register(const SignalActor: IObservable; const Message: string; const SlotType: TSlotType; const Slot: Spring.TAction<TArray<TValue>>); overload;

    procedure UnregisterSignalActor(const SignalActor: IObservable);

    // IObserver
    procedure Notify(const Sender: IInterface; const Notification: INotification);
  end;

implementation

{ TSlots }

constructor TSlots.Create;
begin
  inherited;

  FLock := TMREWSync.Create;
  FSignalSlots := TCollections.CreateList<TSignalSlot>;
  FObservables := TCollections.CreateSet<Weak<IObservable>>;
end;

destructor TSlots.Destroy;
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

procedure TSlots.Notify(
  const Sender: IInterface;
  const Notification: INotification);
begin
  FLock.BeginRead;
  try
    FSignalSlots.Where(
      function(const SignalSlot: TSignalSlot): Boolean
      begin
        Result :=
          (SignalSlot.SignalActor as IInterface = Sender) and
          (Notification.GetDescription = SignalSlot.FMessage);
      end).ForEach(
      procedure(const SignalSlot: TSignalSlot)
      var
        Params: TArray<TValue>;
      begin
        Params := Notification.GetData.AsType<TArray<TValue>>;
        case SignalSlot.SlotType of
          stSynched:
            TThread.Synchronize(
              nil,
              procedure
              begin
                SignalSlot.FSlot(Params)
              end);
          stNotSynched: SignalSlot.FSlot(Params);
        end;
      end);
  finally
    FLock.EndRead;
  end;
end;

procedure TSlots.Register(
  const SignalActor: IObservable;
  const Message: string;
  const SlotType: TSlotType;
  const Slot: Spring.TAction<TArray<TValue>>);
begin
  if FObservables.Add(SignalActor) then
    SignalActor.RegisterObserver(Self);

  FLock.BeginWrite;
  try
    FSignalSlots.Add(TSignalSlot.Create(SignalActor, Message, SlotType, Slot));

  finally
    FLock.EndWrite;
  end;
end;

procedure TSlots.UnregisterSignalActor(const SignalActor: IObservable);
var
  ToBeDeleted: IEnumerable<TSignalSlot>;
begin
  FLock.BeginWrite;
  try
    ToBeDeleted := FSignalSlots.Where(
      function(const SignalSlot: TSignalSlot): Boolean
      begin
        Result := SignalSlot.SignalActor = SignalActor;
      end);

    ToBeDeleted.ForEach(
      procedure(const SignalSlot: TSignalSlot)
      var
        Observable: IObservable;
      begin
        if not Supports(SignalSlot.SignalActor, IObservable, Observable) then
          raise ESlots.Create('SignalActor must be an IObservable');

        Observable.UnregisterObserver(Self);
      end);

    FSignalSlots.ExtractRange(ToBeDeleted.ToArray);
  finally
    FLock.EndWrite;
  end;

end;

procedure TSlots.Register(
  const SignalActor: IObservable;
  const Message: string;
  const SlotType: TSlotType;
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
    raise ESlots.CreateFmt('Method %s not found', [MethodName]);

  if Method.MethodKind <> TMethodKind.mkProcedure then
    raise ESlots.CreateFmt('Method %s is not a procedure', [MethodName]);

  Proc := procedure(const Values: TArray<TValue>)
    var
      MappedValues: TArray<TValue>;
      RequiredParams: Integer;
      Index: Integer;
    begin
      try
        if Assigned(MapParams) then
          MappedValues := MapParams(Values)
        else
        begin
          RequiredParams := Length(Method.GetParameters);
          if Length(Values) = RequiredParams then
            MappedValues := Values
          else
          begin
            SetLength(MappedValues, RequiredParams);
            for Index := 0 to RequiredParams - 1 do
              MappedValues[Index] := Values[Index];
          end;
        end;

        Method.Invoke(SlotActor, MappedValues);
      except
        on E: Exception do
          raise ESlots.CreateFmt('Slot failed. Method: %s Error Message: %s', [MethodName, E.Message]);
      end;
    end;

  Register(SignalActor, Message, SlotType, Proc);
end;

{ TSlots.TSignalSlot }

constructor TSlots.TSignalSlot.Create(
  const SignalActor: IObservable;
  const Message: string;
  const SlotType: TSlotType;
  const Slot: Spring.TAction<TArray<TValue>>);
begin
  FSignalActor := SignalActor;
  FMessage := Message;
  FSlot := Slot;
  FSlotType := SlotType;
end;

function TSlots.TSignalSlot.GetSignalActor: IObservable;
begin
  Result := FSignalActor;
end;

end.

