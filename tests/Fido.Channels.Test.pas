unit Fido.Channels.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.Diagnostics,
  DUnitX.TestFramework,

  Spring.Collections,

  Fido.Channels,
  Fido.Channels.Intf;

type
  [TestFixture]
  TChannelsTests = class
  public
    [Test]
    procedure ChannelBlocksReceiveUntilDataIsSent;

    [Test]
    procedure ChannelTryReceiveReturnsFalseIfDataIsNotQueued;

    [Test]
    procedure ChannelTryReceiveReturnsTrueIfDataIsQueued;

    [Test]
    procedure ChannelTrySendReturnsFalseIfChannelIsFull;

    [Test]
    procedure ChannelTrySendReturnsTrueIfChannelIsNotFull;

    [Test]
    procedure ChannelBlocksSendUntilQueueIsAvailable;

    [Test]
    procedure BufferedChannelDoesNotBlockSendWhileQueueIsAvailable;

    [Test]
    procedure BufferedChannelBlocksSendWhenQueueIsAvailable;

    [Test]
    procedure ChannelIsClosedWhenCloseIsCalledAndQueueIsEmpty;

    [Test]
    procedure ChannelDoesNotBlockTryReceiveEvenWhenQueueISEmpty;

    [Test]
    procedure ChannelGetEnumeratorRaisesEChannelWhenChannelIsNotClosed;

    [Test]
    procedure ChannelGetEnumeratorWorksWhenChannelIsClosed;

    [Test]
    procedure ChannelSendRaisesEChannelWhenChannelIsClosed;

    [Test]
    procedure ChannelReceiveRaisesEChannelWhenChannelIsClosed;

    [Test]
    procedure SelectRunBlocksUntilAllChannelsAreReceivedWithNoFeedback;

    [Test]
    procedure SelectRunBlocksUntilAllChannelsAreReceivedWithFeedback;

    [Test]
    procedure SelectRunWithTimeoutBlocksUntilAllChannelsAreReceived;

    [Test]
    procedure SelectRunWithTimeoutBlocksTimeout;

    [Test]
    procedure SelectRunWithDefaultExecuteCaseThatIsTriggered;

    [Test]
    procedure SelectRunWithDefaultExecuteDefaultIfNoCaseIsTriggered;
  end;

implementation

procedure TChannelsTests.BufferedChannelBlocksSendWhenQueueIsAvailable;
begin
  var Channel := Channels.Make<Boolean>(2);

  var StopWatch := TStopwatch.StartNew;

  Channel.Send(True);

  TTask.Run(
    procedure
    begin
      Sleep(100);
      Channel.Send(True);
    end);

  Channel.Receive;

  Channel.Receive;

  StopWatch.Stop;

  Assert.IsTrue(StopWatch.ElapsedMilliseconds > 100);
end;

procedure TChannelsTests.BufferedChannelDoesNotBlockSendWhileQueueIsAvailable;
begin
  var Channel := Channels.Make<Boolean>(2);

  var StopWatch := TStopwatch.StartNew;

  Channel.Send(True);

  StopWatch.Stop;

  Assert.IsTrue(StopWatch.ElapsedMilliseconds < 10);

  StopWatch.Reset;
  Stopwatch := Stopwatch.StartNew;

  TTask.Run(
    procedure
    begin
      Sleep(100);
      Channel.Send(True);
    end);

  Channel.Receive;
  Channel.Receive;

  StopWatch.Stop;

  Assert.IsTrue(StopWatch.ElapsedMilliseconds > 100);
end;

procedure TChannelsTests.ChannelBlocksReceiveUntilDataIsSent;
begin
  var Channel := Channels.Make<Boolean>;

  var StopWatch := TStopwatch.StartNew;

  TTask.Run(
    procedure
    begin
      Sleep(100);
      Channel.Send(True);
    end);

  Channel.Receive;

  StopWatch.Stop;

  Assert.IsTrue(StopWatch.ElapsedMilliseconds > 100);
end;

procedure TChannelsTests.ChannelBlocksSendUntilQueueIsAvailable;
begin
  var Channel := Channels.Make<Boolean>;

  var StopWatch := TStopwatch.StartNew;

  Channel.Send(True);

  TTask.Run(
    procedure
    begin
      Sleep(100);
      Channel.Receive;
    end);

  Channel.Send(True);

  Channel.Receive;

  StopWatch.Stop;

  Assert.IsTrue(StopWatch.ElapsedMilliseconds > 100);
end;

procedure TChannelsTests.ChannelDoesNotBlockTryReceiveEvenWhenQueueISEmpty;
begin
  var Channel := Channels.Make<Boolean>;

  var Received: Boolean;

  Assert.IsFalse(Channel.TryReceive(Received));
end;

procedure TChannelsTests.ChannelGetEnumeratorRaisesEChannelWhenChannelIsNotClosed;
begin
  var Channel := Channels.Make<Boolean>(2);

  Channel.Send(True);
  Channel.Send(True);

  Assert.WillRaise(procedure
    begin
      Channel.GetEnumerator;
    end,
    EChannel);
end;

procedure TChannelsTests.ChannelGetEnumeratorWorksWhenChannelIsClosed;
var
  Enumerator: IEnumerator<Boolean>;
begin
  var Channel := Channels.Make<Boolean>(2);

  Channel.Send(True);
  Channel.Send(False);

  Channel.Close;

  Assert.WillNotRaise(procedure
    begin
      Enumerator := Channel.GetEnumerator;
    end,
    EChannel);

  Assert.IsTrue(Enumerator.MoveNext);
  Assert.IsTrue(Enumerator.Current);
  Assert.IsTrue(Enumerator.MoveNext);
  Assert.IsFalse(Enumerator.Current);
  Assert.IsFalse(Enumerator.MoveNext);
end;

procedure TChannelsTests.ChannelIsClosedWhenCloseIsCalledAndQueueIsEmpty;
begin
  var Channel := Channels.Make<Boolean>(2);

  Channel.Send(True);
  Channel.Send(True);

  Channel.Close;

  Assert.IsFalse(Channel.Closed);

  Channel.Receive;
  Channel.Receive;

  Assert.IsTrue(Channel.Closed);
end;

procedure TChannelsTests.ChannelReceiveRaisesEChannelWhenChannelIsClosed;
begin
  var Channel := Channels.Make<Boolean>(2);

  Channel.Send(True);
  Channel.Close;

  Channel.Receive;

  Assert.WillRaise(procedure
    begin
      Channel.Receive;
    end,
    EChannel)
end;

procedure TChannelsTests.ChannelSendRaisesEChannelWhenChannelIsClosed;
begin
  var Channel := Channels.Make<Boolean>(2);

  Channel.Send(True);
  Channel.Send(True);

  Channel.Receive;

  Channel.Close;

  Assert.WillRaise(procedure
    begin
      Channel.Send(True);
    end,
    EChannel)
end;

procedure TChannelsTests.ChannelTryReceiveReturnsFalseIfDataIsNotQueued;
begin
  var Channel := Channels.Make<Boolean>;
  var Value: Boolean;

  Assert.IsFalse(Channel.TryReceive(Value));
end;

procedure TChannelsTests.ChannelTryReceiveReturnsTrueIfDataIsQueued;
begin
  var Channel := Channels.Make<string>;
  var Value: string;

  Channel.Send('test');

  Assert.IsTrue(Channel.TryReceive(Value));
  Assert.AreEqual('test', Value);
end;

procedure TChannelsTests.ChannelTrySendReturnsFalseIfChannelIsFull;
begin
  var Channel := Channels.Make<string>;
  var Value: string;

  Channel.Send('test');

  Assert.IsFalse(Channel.TrySend('another test'));
  Assert.AreEqual('test', Channel.Receive);
  Assert.IsFalse(Channel.TryReceive(Value));
end;

procedure TChannelsTests.ChannelTrySendReturnsTrueIfChannelIsNotFull;
begin
  var Channel := Channels.Make<string>;
  var Value: string;

  Channel.Send('test');

  Assert.IsFalse(Channel.TrySend('another test'));
  Assert.AreEqual('test', Channel.Receive);
  Assert.IsFalse(Channel.TryReceive(Value));
end;

procedure TChannelsTests.SelectRunBlocksUntilAllChannelsAreReceivedWithFeedback;
begin
  var C1 := Channels.Make<string>;
  var C2 := Channels.Make<Real>;
  var StopWatch := TStopwatch.StartNew;
  var C1Feedback: Boolean := False;
  var C2Feedback: Boolean := False;

  TTask.Run(procedure
    begin
      Sleep(200);
      C1.Send('');
    end);

  TTask.Run(procedure
    begin
      Sleep(100);
      C2.Send(1);
    end);

  var Select := Channels.Select;
  Channels.&Case<string>(Select, C1.AsReceiver, procedure (const Value: string)
    begin
      C1Feedback := True;
    end);
  Channels.&Case<Real>(Select, C2.AsReceiver, procedure (const Value: Real)
    begin
      C2Feedback := True;
    end);
  Select.Run;

  StopWatch.Stop;

  Assert.IsTrue(StopWatch.ElapsedMilliseconds > 80);
  Assert.IsTrue(StopWatch.ElapsedMilliseconds < 300);
  Assert.IsTrue(C1Feedback);
  Assert.IsTrue(C2Feedback);
end;

procedure TChannelsTests.SelectRunBlocksUntilAllChannelsAreReceivedWithNoFeedback;
begin
  var C1 := Channels.Make<string>;
  var C2 := Channels.Make<Boolean>;
  var StopWatch := TStopwatch.StartNew;

  TTask.Run(procedure
    begin
      Sleep(200);
      C1.Send('');
    end);

  TTask.Run(procedure
    begin
      Sleep(100);
      C2.Send(True);
    end);

  var Select := Channels.Select;
  Channels.&Case<string>(Select, C1.AsReceiver);
  Channels.&Case<Boolean>(Select, C2.AsReceiver);
  Select.Run;

  StopWatch.Stop;

  Assert.IsTrue(StopWatch.ElapsedMilliseconds > 80);
  Assert.IsTrue(StopWatch.ElapsedMilliseconds < 300);
end;

procedure TChannelsTests.SelectRunWithTimeoutBlocksUntilAllChannelsAreReceived;
begin
  var C1 := Channels.Make<string>;
  var C2 := Channels.Make<Boolean>;
  var C3 := Channels.Make<Integer>;
  var StopWatch := TStopwatch.StartNew;
  var C1Feedback: Boolean := False;
  var C2Feedback: Boolean := False;
  var C3Feedback: Boolean := False;
  var TimeoutFeedback: Boolean := False;

  TTask.Run(procedure
    begin
      Sleep(200);
      C1.Send('');
    end);

  TTask.Run(procedure
    begin
      Sleep(100);
      C2.Send(True);
    end);

  TTask.Run(procedure
    begin
      Sleep(50);
      C3.Send(1);
    end);

  var Select := Channels.Select;
  Channels.&Case<string>(Select, C1.AsReceiver, procedure (const Value: string)
    begin
      C1Feedback := True;
    end);
  Channels.&Case<Boolean>(Select, C2.AsReceiver, procedure (const Value: Boolean)
    begin
      C2Feedback := True;
    end);
  Channels.&Case<Integer>(Select, C3.AsReceiver, procedure (const Value: Integer)
    begin
      C3Feedback := True;
    end);
  Select.Run(300, procedure
    begin
      TimeoutFeedback := True;
    end);

  StopWatch.Stop;

  Assert.IsTrue(StopWatch.ElapsedMilliseconds > 80);
  Assert.IsTrue(StopWatch.ElapsedMilliseconds < 300);
  Assert.IsTrue(C1Feedback);
  Assert.IsTrue(C2Feedback);
  Assert.IsTrue(C3Feedback);
  Assert.IsFalse(TimeoutFeedback);
end;

procedure TChannelsTests.SelectRunWithTimeoutBlocksTimeout;
begin
  var C1 := Channels.Make<string>;
  var C2 := Channels.Make<Boolean>;
  var StopWatch := TStopwatch.StartNew;
  var C1Feedback: Boolean := False;
  var C2Feedback: Boolean := False;
  var TimeoutFeedback: Boolean := False;

  TTask.Run(procedure
    begin
      Sleep(200);
      C1.Send('');
    end);

  TTask.Run(procedure
    begin
      Sleep(100);
      C2.Send(True);
    end);

  var Select := Channels.Select;
  Channels.&Case<string>(Select, C1.AsReceiver, procedure (const Value: string)
    begin
      C1Feedback := True;
    end);
  Channels.&Case<Boolean>(Select, C2.AsReceiver, procedure (const Value: Boolean)
    begin
      C2Feedback := True;
    end);
  Select.Run(150, procedure
    begin
      TimeoutFeedback := True;
    end);

  StopWatch.Stop;

  Assert.IsTrue(StopWatch.ElapsedMilliseconds > 80);
  Assert.IsTrue(StopWatch.ElapsedMilliseconds < 200);
  Assert.IsFalse(C1Feedback);
  Assert.IsTrue(C2Feedback);
  Assert.IsTrue(TimeoutFeedback);
end;

procedure TChannelsTests.SelectRunWithDefaultExecuteCaseThatIsTriggered;
begin
  var C1 := Channels.Make<string>;
  var C2 := Channels.Make<Boolean>;
  var C1Feedback: Boolean := False;
  var C2Feedback: Boolean := False;
  var DefaultFeedback: Boolean := False;

  C1.Send('');

  var Select := Channels.Select;
  Channels.&Case<string>(Select, C1.AsReceiver, procedure (const Value: string)
    begin
      C1Feedback := True;
    end);
  Channels.&Case<Boolean>(Select, C2.AsReceiver, procedure (const Value: Boolean)
    begin
      C2Feedback := True;
    end);
  Select.Run(procedure
    begin
      DefaultFeedback := True;
    end);

  Assert.IsTrue(C1Feedback);
  Assert.IsFalse(C2Feedback);
  Assert.IsFalse(DefaultFeedback);
end;

procedure TChannelsTests.SelectRunWithDefaultExecuteDefaultIfNoCaseIsTriggered;
begin
  var C1 := Channels.Make<string>;
  var C2 := Channels.Make<Boolean>;
  var C1Feedback: Boolean := False;
  var C2Feedback: Boolean := False;
  var DefaultFeedback: Boolean := False;

  var Select := Channels.Select;
  Channels.&Case<string>(Select, C1.AsReceiver, procedure (const Value: string)
    begin
      C1Feedback := True;
    end);
  Channels.&Case<Boolean>(Select, C2.AsReceiver, procedure (const Value: Boolean)
    begin
      C2Feedback := True;
    end);
  Select.Run(procedure
    begin
      DefaultFeedback := True;
    end);

  Assert.IsFalse(C1Feedback);
  Assert.IsFalse(C2Feedback);
  Assert.IsTrue(DefaultFeedback);
end;

initialization
  TDUnitX.RegisterTestFixture(TChannelsTests);
end.
