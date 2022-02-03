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

  Spring,

  Redis.Values,
  Redis.Commons,
  Redis.Client,

  Fido.Redis.Client.Intf;

type
  TFidoRedisClient = class(TInterfacedObject, IFidoRedisClient)
  private
    FRedisClient: IRedisClient;
  public
    constructor Create(const RedisClient: IRedisClient);

    function DEL(const Key: string): Integer;
    function GET(const Key: string): Nullable<string>;
    function &SET(const Key: string; const Value: string): Boolean;
    function RPOP(const Key: string): Nullable<string>;
    function LPUSH(const Key: string; const Value: string): Integer;
    function PUBLISH(const Key: string; const Value: string): Integer;
    procedure SUBSCRIBE(const Channel: string;
      aCallback: TProc<string, string>;
      aContinueOnTimeoutCallback: TRedisTimeoutCallback = nil;
      aAfterSubscribe: TProc = nil);

    function ExecuteWithStringResult(const RedisCommand: IRedisCommand): TRedisString;
  end;

implementation

{ TFidoRedisClient }

constructor TFidoRedisClient.Create(const RedisClient: IRedisClient);
begin
  inherited Create;

  Guard.CheckNotNull(RedisClient, 'RedisClient');
  FRedisClient := RedisClient;
end;

function TFidoRedisClient.DEL(const Key: string): Integer;
begin
  Result := FRedisClient.DEL([Key]);
end;

function TFidoRedisClient.ExecuteWithStringResult(const RedisCommand: IRedisCommand): TRedisString;
begin
  Result := FRedisClient.ExecuteWithStringResult(RedisCommand);
end;

function TFidoRedisClient.GET(const Key: string): Nullable<string>;
var
  RedisValue: TRedisNullable<string>;
  ResultValue: Nullable<string>;
begin
  RedisValue := FRedisClient.GET(Key);
  Result := ResultValue;

  if RedisValue.HasValue then
    Result := RedisValue.Value;
end;

function TFidoRedisClient.LPUSH(
  const Key: string;
  const Value: string): Integer;
begin
  Result := FRedisClient.LPUSH(Key, [Value]);
end;

function TFidoRedisClient.PUBLISH(
  const Key: string;
  const Value: string): Integer;
begin
  Result := FRedisClient.PUBLISH(Key, Value);
end;

function TFidoRedisClient.RPOP(const Key: string): Nullable<string>;
var
  Value: string;
begin
  Result := Nullable<string>.Create(null);
  if FRedisClient.RPOP(Key, Value) then
    Result := Value;
end;

function TFidoRedisClient.&SET(
  const Key: string;
  const Value: string): Boolean;
begin
  Result := FRedisClient.&SET(Key, Value);
end;

procedure TFidoRedisClient.SUBSCRIBE(const Channel: string; aCallback: TProc<string, string>; aContinueOnTimeoutCallback: TRedisTimeoutCallback; aAfterSubscribe: TProc);
begin
  FRedisClient.SUBSCRIBE(
    [Channel],
    aCallback,
    aContinueOnTimeoutCallback,
    aAfterSubscribe)
end;

end.

