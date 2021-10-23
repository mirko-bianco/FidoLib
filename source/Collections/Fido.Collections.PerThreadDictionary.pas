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

unit Fido.Collections.PerThreadDictionary;

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  Winapi.PsAPI,
  Winapi.TlHelp32,
  {$endif}
  {$ifdef POSIX}
  Posix.Pthread,
  {$endif}

  System.SysUtils,
  System.Classes,

  Spring,
  Spring.Collections;

type
  // This dictionary store one item per thread.
  TPerThreadDictionary<T> = class
  type
    TThreadId = Integer;
  strict private
    FLock: IReadWriteSync;
    FFactoryFunc: TFunc<T>;
    FGarbageThread: TThread;
  protected
    FItems: IDictionary<TThreadId, T>;

    function GetThreadsList: TArray<TThreadId>;
    procedure PerformGarbageCollection;
  public
    constructor Create(const Ownership: TDictionaryOwnerships; const FactoryFunc: TFunc<T>); reintroduce;
    destructor Destroy; override;

    function GetCurrent: T;
    procedure ReleaseCurrent;
  end;

implementation

{ TPerThreadDictionary<T> }

function TPerThreadDictionary<T>.GetThreadsList: TArray<TThreadId>;
var
  SnapProcHandle: THandle;
  NextProc: Boolean;
  TThreadEntry: TThreadEntry32;
  PID: Cardinal;
begin
  PID := MainThreadID;
  SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if not (SnapProcHandle <> INVALID_HANDLE_VALUE) then
    Exit;

  try
    TThreadEntry.dwSize := SizeOf(TThreadEntry);
    NextProc := Thread32First(SnapProcHandle, TThreadEntry);
    while NextProc do
    begin
      if TThreadEntry.th32OwnerProcessID = PID then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := TThreadEntry.th32ThreadID;
      end;

      NextProc := Thread32Next(SnapProcHandle, TThreadEntry);
    end;
  finally
    CloseHandle(SnapProcHandle);
  end;
end;

procedure TPerThreadDictionary<T>.PerformGarbageCollection;
var
  LiveThreads: ISet<TThreadId>;
  GarbageThreads: ISet<TThreadId>;
begin
  LiveThreads := TCollections.CreateSet<TThreadId>(GetThreadsList);
  GarbageThreads := TCollections.CreateSet<TThreadId>;

  FLock.BeginWrite;
  try
    FItems.Keys.ForEach(
      procedure(const Id: TThreadId)
      begin
        if not LiveThreads.Contains(Id) then
          GarbageThreads.Add(Id);
      end);

    GarbageThreads.ForEach(
      procedure(const Id: TThreadId)
      begin
        FItems.Remove(Id);
      end);
  finally
    FLock.EndWrite;
  end;
end;

constructor TPerThreadDictionary<T>.Create(
  const Ownership: TDictionaryOwnerships;
  const FactoryFunc: TFunc<T>);
begin
  Guard.CheckTrue(Assigned(FactoryFunc), 'FFactoryFunc');
  inherited Create;
  FLock := TMREWSync.Create;
  FItems := Spring.Collections.TCollections.CreateDictionary<Integer, T>(Ownership);
  FFactoryFunc := FactoryFunc;
  FGarbageThread := TThread.CreateAnonymousThread(
    procedure
    begin
      while true do
      begin
        Sleep(10000);
        PerformGarbageCollection;
      end;
    end);
    FGarbageThread.NameThreadForDebugging('TPerThreadDictionary<T>.GarbageCollection');
    FGarbageThread.Start;
end;

destructor TPerThreadDictionary<T>.Destroy;
begin
  FGarbageThread.Free;
  inherited;
end;

function TPerThreadDictionary<T>.GetCurrent: T;
var
  Item: T;
  Found: Boolean;
begin
  FLock.BeginRead;
  try
    Found := FItems.TryGetValue(GetCurrentThreadId, Item);
  finally
    FLock.EndRead;
  end;
  if not Found then
  begin
    FLock.BeginWrite;
    try
      Item := FFactoryFunc();
      FItems.Add(GetCurrentThreadId, Item);
    finally
      FLock.EndWrite;
    end;
  end;
  Result := Item;
end;

procedure TPerThreadDictionary<T>.ReleaseCurrent;
begin
  FLock.BeginWrite;
  try
    FItems.Remove(GetCurrentThreadId);
  finally
    FLock.EndWrite;
  end;
end;

end.
