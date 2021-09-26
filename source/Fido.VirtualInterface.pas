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

unit Fido.VirtualInterface;

interface

uses
  System.Rtti;

type
  // stops RTTI from complaining that VI doesn't really implement an interface
  // see: https://groups.google.com/forum/#!topic/spring4d/6Su_OqYXXnA
  TVirtualInterface<T: IInterface> = class(TVirtualInterface)
{$IFDEF MSWINDOWS}
  private
    class var FInterfaceTable: PInterfaceTable;
    function GetSelf: T;
  public
    class constructor Create;
    class destructor Destroy;
{$ENDIF}
    constructor Create(InvokeEvent: TVirtualInterfaceInvokeEvent);
  end;

implementation

uses
  System.TypInfo
{$IFDEF MSWINDOWS}
  ,WinAPI.Windows
{$ENDIF}
  ;

{ TVirtualInterface<T> }

{$IFDEF MSWINDOWS}
class constructor TVirtualInterface<T>.Create;
var
  NumberOfBytesWritten: NativeUInt;
begin
  New(FInterfaceTable);
  FInterfaceTable.EntryCount := 1;
  FInterfaceTable.Entries[0].IID := GetTypeData(TypeInfo(T)).Guid;
  FInterfaceTable.Entries[0].VTable := nil;
  FInterfaceTable.Entries[0].IOffset := 0;
  FInterfaceTable.Entries[0].ImplGetter := NativeUInt(@TVirtualInterface<T>.GetSelf);

  WriteProcessMemory(GetCurrentProcess, PPointer(NativeInt(TVirtualInterface<T>) + vmtIntfTable),
    @FInterfaceTable, SizeOf(Pointer), NumberOfBytesWritten);
end;

class destructor TVirtualInterface<T>.Destroy;
begin
  Dispose(FInterfaceTable);
end;

function TVirtualInterface<T>.GetSelf: T;
begin
  Self.QueryInterface(FInterfaceTable.Entries[0].IID, Result);
end;

{$ENDIF}

constructor TVirtualInterface<T>.Create(InvokeEvent: TVirtualInterfaceInvokeEvent);
begin
  inherited Create(TypeInfo(T), InvokeEvent);
end;



end.
