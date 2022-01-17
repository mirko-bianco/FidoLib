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

unit Fido.EventsDriven.Listener.Intf;

interface

type
  TConsumerData = record
  private
    FConsumer: TObject;
    FMethodName: string;
  public
    constructor Create(const Consumer: TObject; const MethodName: string);

    property Consumer: TObject read FConsumer;
    property MethodName: string read FMethodName;
  end;

  TPopEvent = reference to function(const Key: string; out EncodedValue: string): Boolean;

  IEventsDrivenListener = interface(IInvokable)
  ['{E99B154A-8854-4C17-9357-B0E13C3DE909}']

    procedure SubscribeTo(const Channel: string; const EventName: string; const ConsumerData: TConsumerData);
    procedure UnsubscribeFrom(const Channel: string; const EventName: string);

    procedure Stop;
  end;

implementation

{ TResponderData }

constructor TConsumerData.Create(const Consumer: TObject; const MethodName: string);
begin
  FConsumer := Consumer;
  FMethodName := MethodName;
end;

end.
