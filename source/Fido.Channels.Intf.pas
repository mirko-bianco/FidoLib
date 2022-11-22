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

unit Fido.Channels.Intf;

interface

uses
  System.SysUtils,
  System.Rtti,

  Spring,
  Spring.Collections,

  Fido.Exceptions;

type
  EChannel = class(EFidoException);

  IChannelSender<T> = interface(IInvokable)
    ['{8C6DEF5B-25E6-460D-8789-06E737A6906B}']

    function TrySend(const Value: T): Boolean;
    procedure Send(const Value: T);
    procedure Close;
    function Closed: Boolean;
  end;

  IChannelReceiver<T> = interface(IInvokable)
    ['{3E488026-8FBE-412A-AEC6-617305DA0486}']

    function Receive: T;
    function TryReceive(out Value: T): Boolean;
    function Closed: Boolean;
    function GetEnumerator: IEnumerator<T>;
  end;

  IChannel<T> = interface(IInvokable)
    ['{CFB023B5-53DB-44FB-95F0-1C11E565FD34}']

    function TrySend(const Value: T): Boolean;
    procedure Send(const Value: T);
    procedure Close;
    function Receive: T;
    function TryReceive(out Value: T): Boolean;
    function Closed: Boolean;
    function GetEnumerator: IEnumerator<T>;

    function AsReceiver: IChannelReceiver<T>;
    function AsSender: IChannelSender<T>;
  end;

  TryReceiveFunc = reference to function(out Value: TValue): Boolean;

  ISelect = interface
    ['{4ABED8E1-1683-47C5-B245-6B2452D2C543}']

    procedure &Case(const Getter: TFunc<TValue>; const TryGetter: TryReceiveFunc; const Action: TAction<TValue>); overload;
    procedure &Case(const Getter: TFunc<TValue>; const TryGetter: TryReceiveFunc) overload;

    // Blocking, waiting for all the cases to happen
    procedure Run; overload;
    // Blocking, waiting until a timeout
    procedure Run(const Timeout: Cardinal; const TimeoutProc: TProc); overload;
    // Non blocking, cases that have already happened and Default otherwise
    procedure Run(const Default: TProc); overload;
  end;

implementation

end.

