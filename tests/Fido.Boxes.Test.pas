unit Fido.Boxes.Test;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  DUnitX.TestFramework,

  Fido.Boxes;

type
  [TestFixture]
  TBoxesTests = class
  public
    [Test]
    procedure TestBox;

    [Test]
    Procedure TestBoxWithUpdaterProc;

    [Test]
    procedure TestReadOnlyBox;

    [Test]
    Procedure TestReadonlyBoxWithUpdaterProc;

  end;

implementation

procedure TBoxesTests.TestBox;
var
  Box: IBox<Boolean>;
begin
  Box := Box<Boolean>.Setup(True);

  TTask.Run(
    procedure
    begin
      Assert.AreEqual<Boolean>(True, Box.Value);
      Box.UpdateValue(False);
    end).Wait;

  Assert.AreEqual<Boolean>(False, Box.Value);
end;

procedure TBoxesTests.TestBoxWithUpdaterProc;
var
  Box: IBox<Boolean>;
begin
  Box := Box<Boolean>.Setup(True);

  TTask.Run(
    procedure
    begin
      Assert.AreEqual<Boolean>(True, Box.Value);
      Box.UpdateValue(procedure(var Value: Boolean)
        begin
          Value := False;
        end);
    end).Wait;

  Assert.AreEqual<Boolean>(False, Box.Value);
end;

procedure TBoxesTests.TestReadOnlyBox;
var
  Box: IReadonlyBox<Boolean>;
  Updater: TBoxUpdater<Boolean>;
begin
  Box := Box<Boolean>.Setup(True, Updater);

  TTask.Run(
    procedure
    begin
      Assert.AreEqual<Boolean>(True, Box.Value);
      Updater(False);
    end).Wait;

  Assert.AreEqual<Boolean>(False, Box.Value);
end;

procedure TBoxesTests.TestReadonlyBoxWithUpdaterProc;
var
  Box: IReadonlyBox<Boolean>;
  UpdaterProc: TBoxUpdaterProc<Boolean>;
begin
  Box := Box<Boolean>.Setup(True, UpdaterProc);

  TTask.Run(
    procedure
    begin
      Assert.AreEqual<Boolean>(True, Box.Value);
      UpdaterProc(procedure(var Value: Boolean)
        begin
          Value := False;
        end);
    end).Wait;

  Assert.AreEqual<Boolean>(False, Box.Value);
end;

initialization
  TDUnitX.RegisterTestFixture(TBoxesTests);
end.
