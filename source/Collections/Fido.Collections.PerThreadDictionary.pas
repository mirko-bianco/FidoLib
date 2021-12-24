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
  System.Threading,
  System.SysUtils,
  System.Classes,
  System.IOUtils,

  Spring,
  Spring.Collections,

  Fido.Collections.PerXDictionary.Intf;

type
  // This dictionary store one item per thread.
  TPerThreadDictionary<T> = class(TInterfacedObject, IPerXDictionary<T>)
  type
    TThreadId = Integer;
  strict private
    FLock: IReadWriteSync;
    FFactoryFunc: TFunc<T>;
    FGarbageTask: ITask;
  protected
    FItems: IDictionary<TThreadId, T>;

    function GetThreadsList: TArray<TThreadId>;
    procedure PerformGarbageCollection;
  public
    constructor Create(const Ownership: TDictionaryOwnerships; const FactoryFunc: TFunc<T>); reintroduce;
    destructor Destroy; override;

    function GetCurrent: T;
    procedure ReleaseCurrent;

    function GetItems: IEnumerable<T>;
  end;

implementation

{ TPerThreadDictionary<T> }

{$ifdef MSWINDOWS}
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
{$endif}
{$ifdef POSIX}
function TPerThreadDictionary<T>.GetThreadsList: TArray<TThreadId>;
var
  List: IList<TThreadId>;
begin
  List := TCollections.CreateList<TThreadId>;
  TCollections.CreateList<string>(TDirectory.GetDirectories(Format('/Proc/%d/task', [MainThreadID]))).ForEach(
    procedure(const Item: string)
    var
      Value: Integer;
    begin
      if TryStrToInt(Item, Value) then
        List.Add(Value);
    end);
  Result := List.ToArray;
end;
{$endif}

procedure TPerThreadDictionary<T>.PerformGarbageCollection;
var
  ThreadIds: TArray<TThreadId>;
  LiveThreads: ISet<TThreadId>;
  GarbageThreads: ISet<TThreadId>;
begin
  LiveThreads := TCollections.CreateSet<TThreadId>(GetThreadsList);
  GarbageThreads := TCollections.CreateSet<TThreadId>;

  if not Assigned(FLock) then
    Exit;

  FLock.BeginRead;
  try
    ThreadIds := FItems.Keys.ToArray;
  finally
    FLock.EndRead;
  end;

  TCollections.CreateList<TThreadId>(ThreadIds).ForEach(
    procedure(const Id: TThreadId)
    begin
      if not LiveThreads.Contains(Id) and
         (Id <> Integer(MainThreadID)) then
        GarbageThreads.Add(Id);
    end);

  FLock.BeginWrite;
  try
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

destructor TPerThreadDictionary<T>.Destroy;
begin
  FGarbageTask.Cancel;
  Sleep(100);
  PerformGarbageCollection;
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

function TPerThreadDictionary<T>.GetItems: IEnumerable<T>;
begin
  Result := FItems.Values;
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
