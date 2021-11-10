unit Fido.Logging.Appenders.PermanentFile;

interface

uses
  System.Classes,
  System.SysUtils,

  Spring.Logging.Appenders;

type
  TPermanentFileLogAppender = class(TStreamLogAppender)
  private
    procedure SetFileName(const Value: string);
  public
    constructor Create;
    property FileName: string write SetFileName;
  end;

implementation

{ TPermanentFileLogAppender }

constructor TPermanentFileLogAppender.Create;
begin
  inherited CreateInternal(True, nil);
end;

procedure TPermanentFileLogAppender.SetFileName(const Value: string);
var
  Stream: TStream;
begin
  if not FileExists(Value) then
    Stream := TFileStream.Create(Value, fmCreate or fmShareDenyWrite)
  else
    Stream := TFileStream.Create(Value, fmOpenWrite or fmShareDenyWrite);
  Stream.Seek(0, soEnd);
  SetStream(Stream);
end;

end.