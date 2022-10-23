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

unit Fido.Boxes;

interface

uses
  System.SyncObjs;

type
  IReadonlyBox<T> = interface(IInvokable)
    ['{377DD5A7-72DA-495C-9FDF-A0985D08F32D}']

    function Value: T;
  end;

  IBox<T> = interface(IReadonlyBox<T>)
    ['{35CA4A5D-2E2F-4BA1-9930-7F71459D76D5}']

    procedure UpdateValue(const Value: T);
  end;

  TBox<T> = class(TInterfacedObject, IReadonlyBox<T>, IBox<T>)
  private var
    FLock: TLightweightMREW;
    FValue: T;
  public
    constructor Create(const Value: T); overload;

    function Value: T;

    procedure UpdateValue(const Value: T);
  end;

  TBoxUpdater<T> = reference to procedure(const Value: T);

  Box<T> = record
    class function Setup(const Value: T; out Updater: TBoxUpdater<T>): IReadonlyBox<T>; overload; static;
    class function Setup(const Value: T): IBox<T>; overload; static;
  end;

implementation

{ Box<T> }

class function Box<T>.Setup(const Value: T; out Updater: TBoxUpdater<T>): IReadonlyBox<T>;
var
  Box: IBox<T>;
begin
  Box := TBox<T>.Create(Value);
  Updater :=
    procedure(const Value: T)
    begin
      Box.UpdateValue(Value);
    end;
  Result := Box;
end;

class function Box<T>.Setup(const Value: T): IBox<T>;
begin
  Result := TBox<T>.Create(Value);
end;

{ TBox<T> }

constructor TBox<T>.Create(const Value: T);
begin
  inherited Create;
  FValue := Value;
end;


procedure TBox<T>.UpdateValue(const Value: T);
begin
  FLock.BeginWrite;
  try
    FValue := Value;
  finally
    FLock.EndWrite;
  end;
end;

function TBox<T>.Value: T;
begin
  FLock.BeginRead;
  try
    Result := FValue;
  finally
    FLock.EndRead;
  end;
end;

end.
