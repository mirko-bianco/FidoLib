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

unit Fido.Memory.EventsDriven.Broker.PubSub;

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.Classes,
  System.Generics.Collections,

  Spring,
  Spring.Collections,

  Fido.Functional,
  Fido.Functional.Tries,
  Fido.Functional.Ifs,
  Fido.EventsDriven.Consumer.PubSub.Intf,
  Fido.EventsDriven.Broker.PubSub.Intf;

type
  TAbstractMemoryPubSubEventsDrivenBroker<PayloadType> = class abstract(TInterfacedObject, IPubSubEventsDrivenBroker<PayloadType>)
  private
    FLock: TLightweightMREW;
    FNotifications: IDictionary<string, IDictionary<IPubSubEventsDrivenConsumer<PayloadType>, TProc<string, PayloadType>>>;
  protected
    procedure Run(const Proc: TProc); virtual; abstract;
  public
    constructor Create;

    function Push(const Key: string; const Payload: PayloadType; const Timeout: Cardinal = INFINITE): Context<Boolean>;

    procedure Subscribe(const Consumer: IPubSubEventsDrivenConsumer<PayloadType>; const Key: string; const OnNotify: TProc<string, PayloadType>);
    procedure Unsubscribe(const Consumer: IPubSubEventsDrivenConsumer<PayloadType>; const Key: string);

    procedure Stop(const Consumer: IPubSubEventsDrivenConsumer<PayloadType>);
  end;

  TMemoryPubSubEventsDrivenBroker<PayloadType> = class(TAbstractMemoryPubSubEventsDrivenBroker<PayloadType>)
  protected
    procedure Run(const Proc: TProc); override;
  end;

  TSynchedMemoryPubSubEventsDrivenBroker<PayloadType> = class(TAbstractMemoryPubSubEventsDrivenBroker<PayloadType>)
  protected
    procedure Run(const Proc: TProc); override;
  end;

implementation

{ TAbstractMemoryPubSubEventsDrivenBroker<PayloadType> }

constructor TAbstractMemoryPubSubEventsDrivenBroker<PayloadType>.Create;
begin
  inherited;

  FNotifications := TCollections.CreateDictionary<string, IDictionary<IPubSubEventsDrivenConsumer<PayloadType>, TProc<string, PayloadType>>>;
end;

function TAbstractMemoryPubSubEventsDrivenBroker<PayloadType>.Push(
  const Key: string;
  const Payload: PayloadType;
  const Timeout: Cardinal): Context<Boolean>;
var
  Events: IDictionary<IPubSubEventsDrivenConsumer<PayloadType>, TProc<string, PayloadType>>;
  EventsArray: TArray<TProc<string, PayloadType>>;
  SuccessFlag: Boolean;

begin
  Result := False;

  Result := ThenElse.New(TryOut<Boolean>.New(function: Boolean
    begin
      FLock.BeginRead;
      if not FNotifications.TryGetValue(Key, Events) then
        Exit(False);

      EventsArray := Events.Values.ToArray;
      Result := True;
    end).Match(procedure
    begin
      FLock.EndRead;
    end)).&Then<Boolean>(Context<Boolean>.New(function: Boolean
    begin
      TCollections.CreateList<TProc<string, PayloadType>>(EventsArray).ForEach(
        procedure(const Event: TProc<string, PayloadType>)
        begin
          Run(procedure
            begin
              Event(Key, Payload);
            end);
        end);
      Result := True;
    end));
end;

procedure TAbstractMemoryPubSubEventsDrivenBroker<PayloadType>.Stop(const Consumer: IPubSubEventsDrivenConsumer<PayloadType>);
begin
  TryOut<Void>.New(function: Void
    begin
      FLock.BeginWrite;
      FNotifications.Keys.ForEach(
        procedure(const Key: string)
        begin
          FNotifications.Items[Key].Remove(Consumer);
        end);
    end).Match(procedure
    begin
      FLock.EndWrite;
    end).Value;
end;

procedure TAbstractMemoryPubSubEventsDrivenBroker<PayloadType>.Subscribe(
  const Consumer: IPubSubEventsDrivenConsumer<PayloadType>;
  const Key: string;
  const OnNotify: TProc<string, PayloadType>);
var
  Events: IDictionary<IPubSubEventsDrivenConsumer<PayloadType>, TProc<string, PayloadType>>;
begin
  TryOut<Void>.New(function: Void
    begin
      FLock.BeginWrite;
      if not FNotifications.TryGetValue(Key, Events) then
      begin
        Events := TCollections.CreateDictionary<IPubSubEventsDrivenConsumer<PayloadType>, TProc<string, PayloadType>>;
        FNotifications.Items[Key] := Events;
      end;

      Events.Items[Consumer] := OnNotify;
    end).Match(procedure
    begin
      FLock.EndWrite;
    end).Value;
end;

procedure TAbstractMemoryPubSubEventsDrivenBroker<PayloadType>.Unsubscribe(
  const Consumer: IPubSubEventsDrivenConsumer<PayloadType>;
  const Key: string);
var
  Events: IDictionary<IPubSubEventsDrivenConsumer<PayloadType>, TProc<string, PayloadType>>;
begin
  TryOut<Void>.New(function: Void
    begin
      FLock.BeginWrite;
      if not FNotifications.TryGetValue(Key, Events) then
        Exit;

      Events.Remove(Consumer);
    end).Match(procedure
    begin
      FLock.EndWrite;
    end).Value;
end;

{ TMemoryPubSubEventsDrivenBroker<PayloadType> }

procedure TMemoryPubSubEventsDrivenBroker<PayloadType>.Run(const Proc: TProc);
begin
  Proc();
end;

{ TSynchedMemoryPubSubEventsDrivenBroker<PayloadType> }

procedure TSynchedMemoryPubSubEventsDrivenBroker<PayloadType>.Run(const Proc: TProc);
begin
  TThread.Synchronize(
    nil,
    procedure
    begin
      Proc();
    end);
end;

end.
