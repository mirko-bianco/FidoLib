unit Fido.Logging.Appenders.PermanentFile;

interface

uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.TypInfo,
  System.Rtti,

  Spring,
  Spring.Logging,
  Spring.Logging.ResourceStrings,
  Spring.Logging.Appenders,

  Fido.JSON.Mapping,
  Fido.JSON.Marshalling;

type
  TPermanentFileLogAppender = class(TStreamLogAppender)
  private
    procedure SetFilename(const Value: string);
  strict protected
    function FormatBuffer(const event: TLogEvent): TBytes; override;
  public
    constructor Create(const Filename: string; const DateTimeFormat: string = 'yyyy-mm-dd hh:nn:ss:zzz');
  end;

implementation

{ TPermanentFileLogAppender }

constructor TPermanentFileLogAppender.Create(const Filename: string; const DateTimeFormat: string);
begin
  inherited CreateInternal(True, nil);
  SetFilename(Filename);

  MappingsUtilities.RegisterPrimitive<TDateTime>(
    function(const Value: TDateTime): string
    begin
      Result := FormatDateTime(DateTimeFormat, Value);
    end,
    function(const Value: string; const TypInfo: pTypeInfo): TDateTime
    begin
      Result := 0;
    end,
    'Logging');
end;

function TPermanentFileLogAppender.FormatBuffer(const event: TLogEvent): TBytes;
begin
  Result := Encoding.GetBytes(JSONMarshaller.From<TLogEvent>(event) + sLineBreak);
end;

procedure TPermanentFileLogAppender.SetFilename(const Value: string);
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

initialization

  MappingsUtilities.RegisterType<TLogEvent>(
    function(const Value: TLogEvent): string
    const
      LEVEL: array[TLogLevel] of string = (
        'Unknown',
        'Verbose',
        'Debug',
        'Text',
        'Info',
        'Warning',
        'Error',
        'Fatal');
      EVENTTYPE: array[TLogEventType] of string = (
        'Text',
        'Value',
        'CallStack',
        'SerializedData',
        'Entering',
        'Leaving');
    var
      JsonObject: IShared<TJSONObject>;
    begin
      JsonObject := Shared.Make(TJSONObject.Create);
      JsonObject.AddPair('TimeStamp', TJSONObject.ParseJSONValue(JSONMarshaller.From<TDateTime>(Value.TimeStamp, 'Logging')));
      JsonObject.AddPair('Level', LEVEL[Value.Level]);
      JsonObject.AddPair('Type', EVENTTYPE[Value.EventType]);
      JsonObject.AddPair('Message', Value.Msg);
      if Assigned(Value.Exception) then
        JsonObject.AddPair('Exception', TJSONObject.ParseJSONValue(JSONMarshaller.From<Exception>(Value.Exception)));
      if not Value.Data.IsEmpty then
        JsonObject.AddPair('Data', TJSONObject.ParseJSONValue(JSONMarshaller.From(Value.Data, Value.Data.TypeInfo)));
      JsonObject.AddPair('Tag', Value.Tag);
      Result := JsonObject.ToJSON;
    end,
    function(const Value: string; const TypInfo: pTypeInfo): TLogEvent
    begin
    end);

end.
