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

unit Fido.Containers;

interface

uses
  System.SysUtils,
  System.TypInfo,

  Spring.Container,

  Fido.VirtualStatement.Intf,
  Fido.VirtualStatement,
  Fido.VirtualQuery.Intf,
  Fido.VirtualQuery;

type
  Containers = record
    class procedure RegisterVirtualStatement<T: IVirtualStatement>(const Container: TContainer); static;

    class procedure RegisterVirtualQuery<TRecord: IInterface; T: IVirtualQuery>(const Container: TContainer); static;
  end;

implementation

class procedure Containers.RegisterVirtualQuery<TRecord, T>(const Container: TContainer);
begin
  Container.RegisterType<T>.DelegateTo(
    function: T
    var
      RInterface: T;
      LIntf: TGUID;
    begin
      LIntf := GetTypeData(TypeInfo(T))^.Guid;
      Supports(TVirtualQuery<TRecord, T>.GetInstance(Container), LIntf, RInterface);
      Result := RInterface;
    end);
end;

class procedure Containers.RegisterVirtualStatement<T>(const Container: TContainer);
begin
  Container.RegisterType<T>.DelegateTo(
    function: T
    var
      RInterface: T;
      LIntf: TGUID;
    begin
      LIntf := GetTypeData(TypeInfo(T))^.Guid;
      Supports(TVirtualStatement<T>.GetInstance(Container), LIntf, RInterface);
      Result := RInterface;
    end);
end;

end.
