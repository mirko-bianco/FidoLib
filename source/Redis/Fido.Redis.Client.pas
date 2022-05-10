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

unit Fido.Redis.Client;

interface

uses
  System.SysUtils,
  System.Variants,
  Generics.Collections,

  Spring,

  Redis.Values,
  Redis.Commons,
  Redis.Client,

  Fido.Utilities,
  Fido.Functional,
  Fido.Functional.Ifs,
  Fido.Redis.Client.Intf;

type
  TFidoRedisClient = class(TInterfacedObject, IFidoRedisClient)
  private type
    TSubscribeStruct = record
      Channel: string;
      aCallback: TProc<string, string>;
      aContinueOnTimeoutCallback: TRedisTimeoutCallback;
      aAfterSubscribe: TProc;
    end;
  private
    FRedisClient: IRedisClient;
    function HasRedisNullableValue(const Value: TRedisNullable<string>): Context<Boolean>;
    function ConvertRedisNullable(const Value: TRedisNullable<string>): Nullable<string>;
    function DoDEL(const Key: string): Integer;
    function DoGET(const Key: string): TRedisNullable<string>;
    function DoLPUSH(const Params: TArray<string>): Integer;
    function DoPUBLISH(const Params: TArray<string>): Integer;
    function DoRPOP(const Key: string): Nullable<string>;
    function DoSET(const Params: TArray<string>): Boolean;
  public
    constructor Create(const RedisClient: IRedisClient);

    function DEL(const Key: string; const Timeout: Cardinal = INFINITE): Context<Integer>;
    function GET(const Key: string; const Timeout: Cardinal = INFINITE): Context<Nullable<string>>;
    function &SET(const Key: string; const Value: string; const Timeout: Cardinal = INFINITE): Context<Boolean>;
    function RPOP(const Key: string; const Timeout: Cardinal = INFINITE): Context<Nullable<string>>;
    function LPUSH(const Key: string; const Value: string; const Timeout: Cardinal = INFINITE): Context<Integer>;
    function PUBLISH(const Key: string; const Value: string; const Timeout: Cardinal = INFINITE): Context<Integer>;
    function SUBSCRIBE(const Channel: string; aCallback: TProc<string, string>; aContinueOnTimeoutCallback: TRedisTimeoutCallback = nil; aAfterSubscribe: TProc = nil): Context<Void>;
  end;

implementation

{ TFidoRedisClient }

constructor TFidoRedisClient.Create(const RedisClient: IRedisClient);
begin
  inherited Create;

  FRedisClient := Utilities.CheckNotNullAndSet(RedisClient, 'RedisClient');
end;

function TFidoRedisClient.DoDEL(const Key: string): Integer;
var
  Client: IRedisClient;
begin
  Client := FRedisClient;

  Result := Client.DEL([Key]);
end;

function TFidoRedisClient.DEL(
  const Key: string;
  const Timeout: Cardinal): Context<Integer>;
begin
  Result := Context<string>.New(Key).MapAsync<Integer>(DoDel, Timeout);
end;

function TFidoRedisClient.HasRedisNullableValue(const Value: TRedisNullable<string>): Context<Boolean>;
var
  LValue: TRedisNullable<string>;
begin
  LValue := Value;
  Result := function: Boolean
    begin
      Result := LValue.HasValue;
    end;
end;

function TFidoRedisClient.ConvertRedisNullable(const Value: TRedisNullable<string>): Nullable<string>;
begin
  Result := Value.Value;
end;

function TFidoRedisClient.DoGET(const Key: string): TRedisNullable<string>;
var
  Client: IRedisClient;
begin
  Client := FRedisClient;

  Result := Client.GET(Key);
end;

function TFidoRedisClient.GET(
  const Key: string;
  const Timeout: Cardinal): Context<Nullable<string>>;
var
  NullValue: Nullable<string>;
begin
  Result := &If<TRedisNullable<string>>.New(Context<string>.New(Key).MapAsync<TRedisNullable<string>>(DoGET, Timeout)).
    Map(HasRedisNullableValue).&Then<Nullable<string>>(ConvertRedisNullable, NullValue);
end;

function TFidoRedisClient.DoLPUSH(const Params: TArray<string>): Integer;
var
  Client: IRedisClient;
begin
  Client := FRedisClient;

  Result := Client.LPUSH(Params[0], [Params[1]]);
end;

function TFidoRedisClient.LPUSH(
  const Key: string;
  const Value: string;
  const Timeout: Cardinal): Context<Integer>;
begin
  Result := Context<TArray<string>>.New([Key, Value]).MapAsync<Integer>(DoLPUSH, Timeout);
end;

function TFidoRedisClient.DoPUBLISH(const Params: TArray<string>): Integer;
var
  Client: IRedisClient;
begin
  Client := FRedisClient;
  Result := Client.PUBLISH(Params[0], Params[1]);
end;

function TFidoRedisClient.PUBLISH(
  const Key: string;
  const Value: string;
  const Timeout: Cardinal): Context<Integer>;
begin
  Result := Context<TArray<string>>.New([Key, Value]).MapAsync<Integer>(DoPUBLISH, Timeout);
end;

function TFidoRedisClient.DoRPOP(const Key: string): Nullable<string>;
var
  Value: string;
  ResultValue: Nullable<string>;
  Client: IRedisClient;
begin
  Client := FRedisClient;
  Result := ResultValue;
  if FRedisClient.RPOP(Key, Value) then
    Result := Value;
end;

function TFidoRedisClient.RPOP(
  const Key: string;
  const Timeout: Cardinal): Context<Nullable<string>>;
begin
  Result := Context<string>.New(Key).MapAsync<Nullable<string>>(DoRPOP, Timeout);
end;

function TFidoRedisClient.DoSET(const Params: TArray<string>): Boolean;
var
  Client: IRedisClient;
begin
  Client := FRedisClient;

  Result := Client.&SET(Params[0], Params[1]);
end;

function TFidoRedisClient.&SET(
  const Key: string;
  const Value: string;
  const Timeout: Cardinal): Context<Boolean>;
begin
  Result := Context<TArray<string>>.New([Key, Value]).MapAsync<Boolean>(DoSET, Timeout);
end;

function TFidoRedisClient.SUBSCRIBE(
  const Channel: string;
  aCallback: TProc<string, string>;
  aContinueOnTimeoutCallback: TRedisTimeoutCallback;
  aAfterSubscribe: TProc): Context<Void>;
var
  Struct: TSubscribeStruct;
  Client: IRedisClient;
begin
  Client := FRedisClient;

  Struct.Channel := Channel;
  Struct.aCallback := aCallback;
  Struct.aContinueOnTimeoutCallback := aContinueOnTimeoutCallback;
  Struct.aAfterSubscribe := aAfterSubscribe;

  Result := Context<TSubscribeStruct>.New(Struct).Map<Void>(Void.MapProc<TSubscribeStruct>(
    procedure(const Struct: TSubscribeStruct)
    begin
      Client.SUBSCRIBE(
        [Struct.Channel],
        Struct.aCallback,
        Struct.aContinueOnTimeoutCallback,
        Struct.aAfterSubscribe)
    end));
end;

end.

