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

unit Fido.Channels.Impl;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Rtti,
  System.Threading,

  Spring,
  Spring.Collections,

  Fido.Channels.Intf;

type
  TChannel<T> = class(TInterfacedObject, IChannelSender<T>, IChannelReceiver<T>, IChannel<T>)
  private
    FBufferSize: Integer;
    FQueue: IQueue<T>;
    FCLosed: Boolean;

    function DoTrySend(const Value: T): Boolean;
    function DoTryReceive(out Value: T): Boolean;
  public
    constructor Create(const BufferSize: Integer = 1);

    function TrySend(const Value: T): Boolean;
    procedure Send(const Value: T);
    procedure Close;
    function Receive: T;
    function TryReceive(out Value: T): Boolean;
    function Closed: Boolean;
    function GetEnumerator: IEnumerator<T>;

    function AsReceiver: IChannelReceiver<T>;
    function AsSender: IChannelSender<T>;
  end;

  TSelect = class(TInterfacedObject, ISelect)
  private
    FGetters: TArray<TFunc<TValue>>;
    FActions: TArray<Action<TValue>>;
    FTryGetters: TArray<TryReceiveFunc>;

    procedure NoAction(const Value: TValue);
    procedure NoFailAction;
  public
    procedure &Case(const Getter: TFunc<TValue>; const TryGetter: TryReceiveFunc; const Action: Action<TValue>); overload;
    procedure &Case(const Getter: TFunc<TValue>; const TryGetter: TryReceiveFunc) overload;

    // Blocking, waiting for all the cases to happen
    procedure Run; overload;
    // Blocking, waiting until a timeout
    procedure Run(const Timeout: Cardinal; const TimeoutProc: TProc); overload;
    // Non blocking, cases that have already happened and Default otherwise
    procedure Run(const Default: TProc); overload;
  end;

implementation

{ TChannel<T> }

function TChannel<T>.AsReceiver: IChannelReceiver<T>;
begin
  Result := Self;
end;

function TChannel<T>.AsSender: IChannelSender<T>;
begin
  Result := Self;
end;

procedure TChannel<T>.Close;
begin
  TMonitor.Enter(Self);
  try
    FClosed := True;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TChannel<T>.Closed: Boolean;
begin
  TMonitor.Enter(Self);
  try
    Result := FClosed and (FQueue.Count = 0);
  finally
    TMonitor.Exit(Self);
  end;
end;

constructor TChannel<T>.Create(const BufferSize: Integer);
begin
  inherited Create;
  FBufferSize := BufferSize;
  FQueue := TCollections.CreateQueue<T>;
  FClosed := False;
end;

function TChannel<T>.DoTryReceive(out Value: T): Boolean;
begin
  Result := False;
  if Closed then
    raise EChannel.Create('Cannot receive from a closed channel.');
  if not TMonitor.TryEnter(Self) then
    Exit;
  try
    Result := FQueue.TryDequeue(Value);
  finally
    TMonitor.Exit(Self);
  end;
end;

function TChannel<T>.DoTrySend(const Value: T): Boolean;
begin
  Result := False;
  if not TMonitor.TryEnter(Self) then
    Exit;
  try
    if FClosed then
      raise EChannel.Create('Cannot send to a closed channel.');
    if (FQueue.Count < FBufferSize) then
    begin
      FQueue.Enqueue(Value);
      Result := True;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TChannel<T>.GetEnumerator: IEnumerator<T>;
begin
  TMonitor.Enter(Self);
  try
    if not FClosed then
      raise EChannel.Create('Cannot get enumerator from an open channel.');

    Result := TCollections.CreateList<T>(FQueue.ToArray).GetEnumerator();
  finally
    TMonitor.Exit(Self);
  end;
end;

function TChannel<T>.Receive: T;
begin
  while not TryReceive(Result) do
    Sleep(1);
end;

procedure TChannel<T>.Send(const Value: T);
begin
  while not DoTrySend(Value) do
    Sleep(1);
end;

function TChannel<T>.TryReceive(out Value: T): Boolean;
begin
  Result := DoTryReceive(Value);
end;

function TChannel<T>.TrySend(const Value: T): Boolean;
begin
  Result := DoTrySend(Value);
end;

{ TSelect }

procedure TSelect.&Case(const Getter: TFunc<TValue>; const TryGetter: TryReceiveFunc; const Action: Action<TValue>);
begin
  SetLength(FGetters, Length(FGetters) + 1);
  FGetters[High(FGetters)] := Getter;
  SetLength(FTryGetters, Length(FTryGetters) + 1);
  FTryGetters[High(FTryGetters)] := TryGetter;
  SetLength(FActions, Length(FActions) + 1);
  FActions[High(FActions)] := Action;
end;

procedure TSelect.&Case(const Getter: TFunc<TValue>; const TryGetter: TryReceiveFunc);
begin
  &Case(Getter, TryGetter, NoAction);
end;

procedure TSelect.NoAction(const Value: TValue);
begin
  // No action
end;

procedure TSelect.NoFailAction;
begin
  // No fail action;
end;

procedure TSelect.Run;
begin
  Run(Infinite, NoFailAction);
end;

procedure TSelect.Run(const Timeout: Cardinal; const TimeoutProc: TProc);
var
  Tasks: TArray<ITask>;
  ActionStarted: TArray<Boolean>;
  PendingCount: Integer;
  Futures: TArray<IFuture<TValue>>;
begin
  var Stopwatch := Now;
  SetLength(Tasks, Length(FGetters));
  SetLength(ActionStarted, Length(FGetters));
  SetLength(Futures, Length(FGetters));

  for var Index := Low(Futures) to High(Futures) do
  begin
    Futures[Index] := TTask.Future<TValue>(FGetters[Index]);
    ActionStarted[Index] := False;
  end;

  var Actions := FActions;
  repeat
    PendingCount := 0;
    for var ActionIndex := Low(Futures) to High(Futures) do
    begin
      var Future := Futures[ActionIndex];
      case Future.Status of
        TTaskStatus.Created,
        TTaskStatus.WaitingToRun,
        TTaskStatus.Running,
        TTaskStatus.WaitingForChildren: Inc(PendingCount);
        TTaskStatus.Completed: begin
          if not ActionStarted[ActionIndex] then
          begin
            ActionStarted[ActionIndex] := True;
            Inc(PendingCount);
            var Value := Future.Value;
            var Action := Actions[ActionIndex];
            Tasks[ActionIndex] := TTask.Run(procedure
              begin
                if Assigned(Action) then
                  Action(Value);
                Action := nil;
                Value := nil;
              end);
          end
          else
          begin
            case Tasks[ActionIndex].Status of
              TTaskStatus.Created,
              TTaskStatus.WaitingToRun,
              TTaskStatus.Running,
              TTaskStatus.WaitingForChildren: Inc(PendingCount);
              TTaskStatus.Completed: ;
              TTaskStatus.Canceled: ;
              TTaskStatus.Exception: ;
            end
          end;
        end;
        TTaskStatus.Canceled: ;
        TTaskStatus.Exception: ;
      end;
    end;
  until (PendingCount = 0) or (Trunc((Now - Stopwatch)*24*60*60*1000) > Timeout);

  if Trunc((Now - Stopwatch)*24*60*60*1000) > Timeout then
    TimeoutProc();
end;

procedure TSelect.Run(const Default: TProc);
var
  Value: TValue;
  Flags: TArray<Boolean>;
  Values: TArray<TValue>;
begin
  var Actions := FActions;

  SetLength(Flags, Length(FActions));
  SetLength(Values, Length(FActions));
  var Flag := False;
  for var Index := Low(FTryGetters) to High(FTryGetters) do
  begin
    Flags[Index] := FTryGetters[Index](Value);
    if Flags[Index] then
    begin
      Flag := Flag or True;
      Values[Index] := Value;
    end;
  end;

  for var Index := Low(Flags) to High(Flags) do
    if Flags[Index] then
      if Assigned(Actions[Index]) then
        Actions[Index](Values[Index]);

  if not Flag then
    Default();
end;

end.
