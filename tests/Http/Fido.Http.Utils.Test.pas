unit Fido.Http.Utils.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  DUnitX.TestFramework,

  IdIOHandler,

  Spring.Mocking,
  Spring.Collections,

  Fido.Testing.Mock.Utils,
  Fido.Http.Utils;

type
  [TestFixture]
  THttpUtilsTests = class
  public
    [Test]
    [TestCase('Empty', ',$')]
    [TestCase('Root', '/,\/$')]
    [TestCase('Simple Path', '/Path,\/Path$')]
    [TestCase('Path with one parameter', '/Path/{AParameter},\/Path\/[\s\S]+$')]
    [TestCase('Path with two parameter', '/Path/{AParameter}/{AnotherParameter},\/Path\/[\s\S]+\/[\s\S]+$')]
    procedure TestTranslatePathToRegEx(const Input: string; const Output: string);

    [Test]
    procedure ParseIOHandlerInputBufferReturnsEmptyStringWhenInputBufferIsEmpty;

    [Test]
    procedure ParseIOHandlerInputBufferReturnsValueWhenInputBufferIsNotEmpty;

    [Test]
    procedure ParseIOHandlerHeadersReturnsAnEmptyMapWhenInputIsEmpty;

    [Test]
    procedure ParseIOHandlerHeadersReturnsHeadersWhenInputIsCorrect;

    [Test]
    procedure ParseIOHandlerHeadersReturnsAnEmptyMapWhenInputIsMalformed;

    [Test]
    procedure ParseIOHandlerTopicReturnsTheTopicWhenPresent;

    [Test]
    procedure ParseIOHandlerTopicReturnsEmptyStringWhenBufferIsEmpty;
  end;

implementation

procedure THttpUtilsTests.ParseIOHandlerInputBufferReturnsValueWhenInputBufferIsNotEmpty;
var
  IOHandler: Mock<TIdIOHandler>;
  Value: string;
begin
  Value := MockUtils.SomeString;
  IOHandler := Mock<TIdIOHandler>.Create;
  IOHandler.Instance.InputBuffer.Write(Value);

  Assert.AreEqual(Value, ParseIOHandlerInputBuffer(IOHandler));
end;

procedure THttpUtilsTests.TestTranslatePathToRegEx(
  const Input: string;
  const Output: string);
begin
  Assert.AreEqual(Output, TranslatePathToRegEx(Input));
end;

procedure THttpUtilsTests.ParseIOHandlerTopicReturnsEmptyStringWhenBufferIsEmpty;
var
  Topic: string;
begin
  Topic := ParseIOHandlerTopic('');

  Assert.AreEqual('', Topic);
end;

procedure THttpUtilsTests.ParseIOHandlerTopicReturnsTheTopicWhenPresent;
var
  Input: string;
  Topic: string;
begin
  Input :=
    'GET /atopic/100/ HTTP/1.1'#$D#$A +
    'Host: 127.0.0.1:8080'#$D#$A +
    'User-Agent: FidoLib websocket client'#$D#$A +
    'Connection: keep-alive, Upgrade'#$D#$A +
    'Upgrade: WebSocket'#$D#$A +
    'Sec-WebSocket-Version: 13'#$D#$A +
    'Sec-WebSocket-Key: bizbFf3AAM/5h4Iox2o9ag=='#$D#$A#$D#$A;

  Topic := ParseIOHandlerTopic(Input);

  Assert.AreEqual('atopic/100', Topic);
end;

procedure THttpUtilsTests.ParseIOHandlerHeadersReturnsAnEmptyMapWhenInputIsMalformed;
var
  Map: IDictionary<string, string>;
  Input: string;
begin
  Input :=
    MockUtils.SomeString + #$D#$A +
    MockUtils.SomeString + #$D#$A +
    MockUtils.SomeString + #$D#$A;

  Map := ParseIOHandlerHeaders(Input);

  Assert.AreEqual(0, Map.Count);
end;

procedure THttpUtilsTests.ParseIOHandlerHeadersReturnsHeadersWhenInputIsCorrect;
var
  Map: IDictionary<string, string>;
  Input: string;
begin
  Input :=
    'GET /atopic/100 HTTP/1.1'#$D#$A +
    'Host: 127.0.0.1:8080'#$D#$A +
    'User-Agent: FidoLib websocket client'#$D#$A +
    'Connection: keep-alive, Upgrade'#$D#$A +
    'Upgrade: WebSocket'#$D#$A +
    'Sec-WebSocket-Version: 13'#$D#$A +
    'Sec-WebSocket-Key: bizbFf3AAM/5h4Iox2o9ag=='#$D#$A#$D#$A;

  Map := ParseIOHandlerHeaders(Input);

  Assert.AreEqual(6, Map.Count);
  Assert.AreEqual('127.0.0.1:8080', Map['Host']);
  Assert.AreEqual('FidoLib websocket client', Map['User-Agent']);
  Assert.AreEqual('keep-alive, Upgrade', Map['Connection']);
  Assert.AreEqual('WebSocket', Map['Upgrade']);
  Assert.AreEqual('13', Map['Sec-WebSocket-Version']);
  Assert.AreEqual('bizbFf3AAM/5h4Iox2o9ag==', Map['Sec-WebSocket-Key']);
end;

procedure THttpUtilsTests.ParseIOHandlerHeadersReturnsAnEmptyMapWhenInputIsEmpty;
var
  Map: IDictionary<string, string>;
begin
  Map := ParseIOHandlerHeaders('');

  Assert.AreEqual(0, Map.Count);
end;

procedure THttpUtilsTests.ParseIOHandlerInputBufferReturnsEmptyStringWhenInputBufferIsEmpty;
var
  IOHandler: Mock<TIdIOHandler>;
begin
  IOHandler := Mock<TIdIOHandler>.Create;
  IOHandler.Setup.Returns<Boolean>(False).When.InputBufferIsEmpty;

  Assert.AreEqual('', ParseIOHandlerInputBuffer(IOHandler));
end;

initialization
  TDUnitX.RegisterTestFixture(THttpUtilsTests);
end.
