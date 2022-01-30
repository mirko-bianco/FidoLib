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

unit Fido.EventsDriven.Subscriber;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,

  Spring,
  Spring.Collections,

  Fido.EventsDriven.Attributes,
  Fido.EventsDriven.Subscriber.Intf,
  Fido.EventsDriven.Listener.Intf;

type
  TEventsDrivenSubscriber = class(TInterfacedObject, IEventsDrivenSubscriber)
  private type
    TChannelEventPair = TPair<string, string>;
  private var
    FListener: IEventsDrivenListener;
    FConsumers: IList<TObject>;
    FChannels: IList<TChannelEventPair>;

    procedure Close;
  public
    constructor Create(const Listener: IEventsDrivenListener);
    destructor Destroy; override;

    procedure RegisterConsumer(const Consumer: TObject);
  end;

implementation

{ TEventsDrivenSubscriber }

procedure TEventsDrivenSubscriber.Close;
var
  UniqueConsumers: IList<TObject>;
begin
  FListener.Stop;
  FChannels.ForEach(
    procedure(const Item: TChannelEventPair)
    begin
      FListener.UnsubscribeFrom(Item.Key, Item.Value);
    end);
  FChannels.Clear;

  UniqueConsumers := TCollections.CreateObjectList<TObject>;
  FConsumers.ForEach(
    procedure(const Item: TObject)
    begin
      if not UniqueConsumers.Contains(Item) and
         not Supports(Item, IInterface) then
        UniqueConsumers.Add(Item);
    end);
  UniqueConsumers.Clear;
  FConsumers.Clear;
end;

constructor TEventsDrivenSubscriber.Create(const Listener: IEventsDrivenListener);
begin
  inherited Create;

  Guard.CheckNotNull(Listener, 'Listener');
  FListener := Listener;

  FConsumers := TCollections.CreateObjectList<TObject>(False);
  FChannels := TCollections.CreateList<TChannelEventPair>;
end;

destructor TEventsDrivenSubscriber.Destroy;
begin
  Close;
  inherited;
end;

procedure TEventsDrivenSubscriber.RegisterConsumer(const Consumer: TObject);
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Registered: Boolean;
begin
  Ctx := TRttiContext.Create;

  RttiType := Ctx.GetType(Consumer.ClassType);

  Registered := False;

  TCollections.CreateList<TRttiMethod>(RttiType.GetMethods).ForEach(
    procedure(const Method: TRttiMethod)
    begin
      TCollections.CreateList<TCustomAttribute>(Method.GetAttributes).
        Where(
          function(const Item: TCustomAttribute): Boolean
          begin
            Result := Item is TriggeredByEventAttribute;
          end).
        ForEach(
          procedure(const Item: TCustomAttribute)
          var
            EDAttribute: TriggeredByEventAttribute;
          begin
            EDAttribute := Item as TriggeredByEventAttribute;

            FConsumers.Add(Consumer);
            FChannels.Add(TChannelEventPair.Create(EDAttribute.Channel, EDAttribute.EventName));
            FListener.SubscribeTo(
              EDAttribute.Channel,
              EDAttribute.EventName,
              TConsumerData.Create(
                Consumer,
                Method.Name));

            Registered := True;
          end);
    end);

  if not Registered then
    Consumer.Free;
end;

end.
