unit Fido.Web.Client.Websocket.Test;

interface
uses
  System.Classes,
  System.Rtti,
  System.SysUtils,
  System.DateUtils,
  DUnitX.TestFramework,
  Rest.Types,
  Data.DBXPlatform,

  Spring,
  Spring.Collections,
  Spring.Mocking,
  Spring.Mocking.Core,

  Fido.Utilities,
  Fido.Json.Marshalling,
  Fido.Testing.Mock.Utils,

  Fido.Web.Client.WebSocket.TcpClient.Intf,
  Fido.Web.Client.WebSocket.Intf,
  Fido.Web.Client.WebSocket;

type

  [TestFixture]
  TClientWebsocketTests = class(TObject)
  public
    [Test]
    procedure Test;

  end;

implementation

{ TClientWebsocketTests }

procedure TClientWebsocketTests.Test;
var
  TcpEngine: Mock<IWebSocketTCPClient>;
  Websocket: IWebSocketClient;

  Url: string;
  CustomHeaders: Shared<TStrings>;
  Started: Boolean;

  TestGetReturn: string;
  GetReturn: string;
begin
  GetReturn := MockUtils.SomeString;
  Url := MockUtils.SomeString;

  TcpEngine := Mock<IWebSocketTCPClient>.Create;
  TcpEngine.Setup.Returns<Boolean>(True).When.Connected;
  TcpEngine.Setup.Returns<string>(GetReturn).When.Get(Arg.IsAny<string>, Arg.IsAny<TStrings>);

  Websocket := TWebsocketClient.Create(TcpEngine);

  CustomHeaders := TStringList.Create;
  Assert.WillNotRaiseAny(
    procedure
    begin
      Websocket.Start(Url, CustomHeaders, procedure(const Message: string)
        begin

        end);
      Started := Websocket.Started;
      TestGetReturn := WebSocket.Get(Url, CustomHeaders);
      WebSocket.Send(MockUtils.SomeString);
      Websocket.Stop;
    end);

  Websocket := nil;

  Assert.AreEqual(GetReturn, TestGetReturn);
  Assert.IsTrue(Started);
  TcpEngine.Received(Times.Once).SetOnError(nil);
  TcpEngine.Received(Times.Once).Connect(Arg.IsAny<string>, Arg.IsAny<TStrings>, Arg.IsAny<string>);
  TcpEngine.Received(Times.Once).Connected;
  TcpEngine.Received(Times.Once).Send(Arg.IsAny<string>);
  TcpEngine.Received(Times.Never).Send(Arg.IsAny<TArray<Byte>>);
  TcpEngine.Received(Times.Once).Get(Arg.IsAny<string>, Arg.IsAny<TStrings>);
  TcpEngine.Received(Times.Once).Close;
end;

initialization
  TDUnitX.RegisterTestFixture(TClientWebsocketTests);
end.
