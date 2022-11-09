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

unit Fido.Channels;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,

  Spring,
  Spring.Collections,

  Fido.Functional,
  Fido.Channels.Intf,
  Fido.Channels.Impl;

type
//"Don't communicate by sharing memory; share memory by communicating." Go koan.
  Channels = class
    class function Make<T>(const BufferSize: Integer = 1): IChannel<T>; static;
    class function Select: ISelect; static;
    // Sets up a case triggered by a message received on the channel that will execute an action
    class procedure &Case<T>(const Select: ISelect; const Channel: IChannelReceiver<T>; const Action: TAction<T>); overload; static;
    // Sets up a case triggered by a message received on the channel
    class procedure &Case<T>(const Select: ISelect; const Channel: IChannelReceiver<T>); overload; static;
  private
    class function ValueAction<T>(const Action: TAction<T>): TAction<TValue>; static;
    class function Getter<T>(const Channel: IChannelReceiver<T>): TFunc<TValue>; static;
    class function TryGetter<T>(const Channel: IChannelReceiver<T>): TryReceiveFunc; static;

    class function LocalAction<T>(const Action: TAction<T>): TAction<TValue>; static;
  end;

implementation

{ Channels }

class function Channels.LocalAction<T>(const Action: TAction<T>): TAction<TValue>;
begin
  Result := nil;
  if Assigned(Action) then
    Result := procedure(const Value: TValue)
      begin
        Action(Value.AsType<T>);
      end;
end;

class function Channels.ValueAction<T>(const Action: TAction<T>): TAction<TValue>;
begin
  Result := LocalAction<T>(Action);
end;

class function Channels.Getter<T>(const Channel: IChannelReceiver<T>): TFunc<TValue>;
begin
  Result := function: TValue
    begin
      Result := TValue.From<T>(Channel.Receive);
    end;
end;

class function Channels.TryGetter<T>(const Channel: IChannelReceiver<T>): TryReceiveFunc;
begin
  Result := function(out Value: TValue): Boolean
    var
      TypedValue: T;
    begin
      Result := Channel.TryReceive(TypedValue);
      Value :=  TValue.From<T>(TypedValue);
    end;
end;

class procedure Channels.&Case<T>(const Select: ISelect; const Channel: IChannelReceiver<T>);
begin
  &Case<T>(Select, Channel, nil);
end;

class procedure Channels.&Case<T>(const Select: ISelect; const Channel: IChannelReceiver<T>; const Action: TAction<T>);
var
  LGetter: TFunc<TValue>;
begin
  LGetter := Getter<T>(Channel);
  Select.&Case(LGetter, TryGetter<T>(Channel), ValueAction<T>(Action));
end;

class function Channels.Make<T>(const BufferSize: Integer): IChannel<T>;
begin
  Result := TChannel<T>.Create(BufferSize);
end;

class function Channels.Select: ISelect;
begin
  Result := TSelect.Create;
end;

end.
