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

unit Fido.Web.Server.WebSocket.Clients;

interface

uses
  System.SysUtils,
  System.SyncObjs,

  idContext,

  Spring,
  Spring.Collections,

  Fido.Web.Server.WebSocket.Client;

type
  TActionProc = reference to procedure(const Client: TWebSocketClient);

  TClients = class
    strict private
      FLock: TLightweightMREW;
      FMap: IDictionary<Integer, TWebSocketClient>;
    public
      constructor Create;
      destructor Destroy; override;

      procedure Add(const Context: TIdContext; const Topic: string);
      procedure Remove(const Context: TIdContext);
      function TryGetByContext(const Context: TIdContext; out Client: TWebSocketClient): Boolean;
      procedure ForEachByTopic(const Topic: string; const Action: TActionProc);
    end;

implementation

{ TClients }

constructor TClients.Create;
begin
  inherited;
  FMap := TCollections.CreateDictionary<Integer, TWebSocketClient>([doOwnsValues]);
end;

destructor TClients.Destroy;
begin
  inherited;
end;

procedure TClients.ForEachByTopic(
  const Topic: string;
  const Action: TActionProc);
begin
  FLock.BeginRead;
  try
    with FMap.Values.GetEnumerator do
      while MoveNext do
        if Current.Topic.ToUpper.Equals(Topic.ToUpper) then
          Action(Current);
  finally
    FLock.EndRead;
  end;
end;

function TClients.TryGetByContext(
  const Context: TIdContext;
  out Client: TWebSocketClient): Boolean;
begin
  FLock.BeginRead;
  try
    Client := nil;
    Result := FMap.TryGetValue(Integer(@Context.Connection.IOHandler), Client);
  finally
    FLock.EndRead;
  end;
end;

procedure TClients.Remove(const Context: TIdContext);
begin
  FLock.BeginWrite;
  try
    FMap.Remove(Integer(@Context.Connection.IOHandler));
  finally
    FLock.EndWrite;
  end;
end;

procedure TClients.Add(
  const Context: TIdContext;
  const Topic: string);
begin
  FLock.BeginWrite;
  try
    FMap[Integer(@Context.Connection.IOHandler)] := TWebSocketClient.Create(Context, Topic);
  finally
    FLock.EndWrite;
  end;
end;



end.
