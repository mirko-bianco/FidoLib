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
  Fido.VirtualQuery,

  Fido.Api.Client.VirtualApi.Intf,
  Fido.Api.Client.VirtualApi.Configuration.Intf,
  Fido.Api.Client.VirtualApi.json;

type
  Containers = record
    class procedure RegisterVirtualStatement<T: IVirtualStatement>(const Container: TContainer; const StatementExecutorServiceName: string = ''); static;

    class procedure RegisterVirtualQuery<TRecord: IInterface; T: IVirtualQuery>(const Container: TContainer; const StatementExecutorServiceName: string = ''); static;

    class procedure RegisterJSONClientApi<Api: IClientVirtualApi; Configuration: IClientVirtualApiConfiguration>(const Container: TContainer); static;
  end;

implementation

class procedure Containers.RegisterJSONClientApi<Api, Configuration>(const Container: TContainer);
{$IFNDEF MSWINDOWS}
var
  GetFunc: TFunc<Configuration, Api>;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Container.RegisterType<Api, TJSONClientVirtualApi<Api, Configuration>>;
  {$ELSE}
  GetFunc := function(Conf: Configuration): Api
    begin
      Result := TJSONClientVirtualApi<Api, Configuration>.Create(Conf).GetSelf;
    end;

  Container.RegisterType<Api>(
    function: Api
    begin
      Result := GetFunc(Container.Resolve<Configuration>);
    end);
  {$ENDIF}
end;

class procedure Containers.RegisterVirtualQuery<TRecord, T>(
  const Container: TContainer;
  const StatementExecutorServiceName: string);
begin
  {$IFDEF MSWINDOWS}
  Container.RegisterType<T>(
    function: T
    var
      RInterface: T;
      LIntf: TGUID;
    begin
      LIntf := GetTypeData(TypeInfo(T))^.Guid;
      Supports(TVirtualQuery<TRecord, T>.GetInstance(Container, StatementExecutorServiceName), LIntf, RInterface);
      Result := RInterface;
    end);
  {$ELSE}
  Container.RegisterType<T>(
    function: T
    begin
      Result := TVirtualQuery<TRecord, T>.GetInstance(Container, StatementExecutorServiceName).GetSelf;
    end);
  {$ENDIF}
end;

class procedure Containers.RegisterVirtualStatement<T>(
  const Container: TContainer;
  const StatementExecutorServiceName: string);
begin
  {$IFDEF MSWINDOWS}
  Container.RegisterType<T>(
    function: T
    var
      RInterface: T;
      LIntf: TGUID;
    begin
      LIntf := GetTypeData(TypeInfo(T))^.Guid;
      Supports(TVirtualStatement<T>.GetInstance(Container, StatementExecutorServiceName), LIntf, RInterface);
      Result := RInterface;
    end);
  {$ELSE}
  Container.RegisterType<T>(
    function: T
    begin
      Result := TVirtualStatement<T>.GetInstance(Container, StatementExecutorServiceName).GetSelf;
    end);
  {$ENDIF}
end;

end.
