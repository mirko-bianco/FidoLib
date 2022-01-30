(*
 * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without Apiriction, including without limitation the rights
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

unit Fido.Api.Client.VirtualApi.Elasticsearch.Document.Dto.Request;

interface

uses
  System.SysUtils,

  Spring.Collections,
  Spring.Logging,
  Spring,

  Fido.Http.Types,
  Fido.OwningObject;

type
  {$M+}
  TElasticsearchDocumentRequest = class
  private
    FLevel: TLogLevel;
    FEventType: TLogEventType;
    FMsg: string;
    FTimeStamp: TDateTime;
    FRaisedException: Exception;
    FContent: TObject;
  public
    constructor Create(const Level: TLogLevel; const EventType: TLogEventType; const Msg: string; const TimeStamp: TDateTime; const RaisedException: Exception; const Content: TObject = nil);
  published
    property Level: TLogLevel read FLevel write FLevel;
    property EventType: TLogEventType read FEventType write FEventType;
    property Msg: string read FMsg write FMsg;
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
    property RaisedException: Exception read FRaisedException write FRaisedException;
    property Content: TObject read FContent write FContent;
  end;
  {$M-}

implementation

{ TElasticsearchDocumentRequest }

constructor TElasticsearchDocumentRequest.Create(
  const Level: TLogLevel;
  const EventType: TLogEventType;
  const Msg: string;
  const TimeStamp: TDateTime;
  const RaisedException: Exception;
  const Content: TObject);
begin
  inherited Create;
  FLevel := Level;
  FEventType := EventType;
  FMsg := Msg;
  FTimeStamp := TimeStamp;

  FRaisedException := RaisedException;

  FContent := Content;
end;

end.
