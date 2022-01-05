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

unit Fido.Collections.PerIWSessionDictionary;

interface

uses
  System.Threading,
  System.SysUtils,
  System.Classes,

  IWApplication,

  Spring,
  Spring.Collections;

type
  // This dictionary store one item per intraweb session.
  TPerIWSessionDictionary<T> = class
  type
    TSessionId = string;
  strict private
    FLock: IReadWriteSync;
    FFactoryFunc: TFunc<T>;
    FGarbageTask: ITask;
  protected
    FItems: IDictionary<TSessionId, T>;

    function GetSessionsList: TArray<TSessionId>;
    procedure PerformGarbageCollection;
  public
    constructor Create(const Ownership: TDictionaryOwnerships; const FactoryFunc: TFunc<T>); reintroduce;
    destructor Destroy; override;

    function GetCurrent: T;
    procedure ReleaseCurrent;
  end;

implementation

function TPerIWSessionDictionary<T>.GetSessionsList: TArray<TSessionId>;
var
  aList: Shared<TStringList>;
begin
  aList := TStringList.Create;
  IWApplication.gSessions.GetList(aList);
  SetLength(result, aList.Value.Count);
  for var i := 0 to aList.Value.Count - 1 do
  begin
     result[i] := aList.Value[i];
  end;
end;

procedure TPerIWSessionDictionary<T>.PerformGarbageCollection;
var
  LiveThreads: ISet<TSessionId>;
  LiveThreads2: ISet<string>;
  GarbageThreads: ISet<TSessionId>;
begin
  LiveThreads := TCollections.CreateSet<TSessionId>(GetSessionsList);
  GarbageThreads := TCollections.CreateSet<TSessionId>;

  if not Assigned(FLock) then
    Exit;

  FLock.BeginWrite;
  try
    FItems.Keys.ForEach(
      procedure(const Id: TSessionId)
      begin
        if not LiveThreads.Contains(Id) then
          GarbageThreads.Add(Id);
      end);

    GarbageThreads.ForEach(
      procedure(const Id: TSessionId)
      begin
        FItems.Remove(Id);
      end);
  finally
    FLock.EndWrite;
  end;
end;

constructor TPerIWSessionDictionary<T>.Create(const Ownership:
    TDictionaryOwnerships; const FactoryFunc: TFunc<T>);
begin
  Guard.CheckTrue(Assigned(FactoryFunc), 'FFactoryFunc');
  inherited Create;
  FLock := TMREWSync.Create;
  FItems := Spring.Collections.TCollections.CreateDictionary<TSessionId, T>(Ownership);
  FFactoryFunc := FactoryFunc;
  FGarbageTask := TTask.Run(
    procedure
    var
      Index: Integer;
      Task: Weak<ITask>;
    begin
      Task := FGarbageTask;
      while true do
      begin
        if not Assigned(FGarbageTask) then
          Exit;
        for Index := 1 to 10 do
          Sleep(100);
        PerformGarbageCollection;
      end;
    end);
end;

destructor TPerIWSessionDictionary<T>.Destroy;
begin
  FGarbageTask.Cancel;
  inherited;
end;

function TPerIWSessionDictionary<T>.GetCurrent: T;
var
  Item: T;
  Found: Boolean;
begin
  FLock.BeginRead;
  try
    Found := FItems.TryGetValue(IWApplication.gGetWebApplicationThreadVar.AppID, Item);
  finally
    FLock.EndRead;
  end;
  if not Found then
  begin
    FLock.BeginWrite;
    try
      Item := FFactoryFunc();
      FItems.Add(IWApplication.gGetWebApplicationThreadVar.AppID, Item);
    finally
      FLock.EndWrite;
    end;
  end;
  Result := Item;
end;

procedure TPerIWSessionDictionary<T>.ReleaseCurrent;
begin
  FLock.BeginWrite;
  try
    FItems.Remove(IWApplication.gGetWebApplicationThreadVar.AppID);
  finally
    FLock.EndWrite;
  end;
end;

end.
