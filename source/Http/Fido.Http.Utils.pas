(*
 * Copyright 2022 Mirko Bianco (email: writetomirko@gmail.com)
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

unit Fido.Http.Utils;

interface

uses
  System.Classes,
  System.SysUtils,
  System.RegularExpressions,

  IdIOHandler,
  IdGlobal,

  Spring,
  Spring.Collections,

  Fido.Http.Types;

procedure StringsToDictionary(const Strings: TStrings; const Dictionary: IDictionary<string, string>);

function ContentTypeToMimeType(const ContentType: string): TMimeType;

function ConvertToRestCommand(const Item: string): THttpMethod;

function TranslatePathToRegEx(Path: string): string;

function ParseIOHandlerInputBuffer(const IOHandler: TIdIOHandler): string;

function ParseIOHandlerHeaders(const Buffer: string): IDictionary<string, string>;

function ParseIOHandlerTopic(const Buffer: string): string;

implementation

procedure StringsToDictionary(
  const Strings: TStrings;
  const Dictionary: IDictionary<string, string>);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Dictionary[Strings.Names[I]] := Strings.ValueFromIndex[I];
end;

function ContentTypeToMimeType(const ContentType: string): TMimeType;
var
  LContentType: string;
  MimeTypeIndex: Integer;
begin
  var Mimetypes: IShared<TStringList> := Shared.Make(TStringList.Create);
  MimeTypes.Delimiter := ';';
  MimeTypes.DelimitedText := ContentType;

  LContentType := '';
  if MimeTypes.Count > 0 then
    LContentType := MimeTypes.Strings[0];

  MimeTypeIndex := -1;
  if LContentType.IsEmpty then
    LContentType := DEFAULTMIME;
  for var StringMimeType in SMimeType do
  begin
    Inc(MimeTypeIndex);
    if LContentType.ToUpper = StringMimeType.ToUpper then
      Break;
  end;
  Result := TMimeType(MimeTypeIndex);
end;

function ConvertToRestCommand(const Item: string): THttpMethod;
var
  I: Integer;
begin
  Result := rmUnknown;

  for I := 0 to Integer(High(SHttpMethod)) do
    if UpperCase(SHttpMethod[THttpMethod(I)]) = UpperCase(Item) then
      Exit(THttpMethod(I));
end;

function TranslatePathToRegEx(Path: string): string;
begin
  Result := StringReplace(Path, '/', '\/', [rfReplaceAll]);
  with TRegEx.Matches(Result, '{[\s\S][^{]+}').GetEnumerator do
  begin
    while MoveNext do
      Result := StringReplace(Result, GetCurrent.Value, '[\s\S]+', []);
    Free;
  end;
  Result := Result + '$';
end;

function ParseIOHandlerInputBuffer(const IOHandler: TIdIOHandler): string;
var
  Bytes: TArray<Byte>;
begin
  Result :=  '';
  if IOHandler.InputBufferIsEmpty then
    Exit;
  IOHandler.InputBuffer.ExtractToBytes(TIdBytes(Bytes));
  Result := IndyTextEncoding_UTF8.GetString(TIdBytes(Bytes));
end;

function ParseIOHandlerHeaders(const Buffer: string): IDictionary<string, string>;
var
  Lines: TArray<string>;
  Line: string;
  LineTokens: TArray<string>;
begin
  Result := TCollections.CreateDictionary<string, string>;

  if Buffer.IsEmpty then
    Exit;

  Lines := Buffer.Split([#13#10]);
  for Line in Lines do
  begin
    LineTokens := Line.Split([': ']);
    if (Length(LineTokens) > 1) then
      Result[Trim(LineTokens[0])] := Trim(LineTokens[1]);
  end;
end;

function ParseIOHandlerTopic(const Buffer: string): string;
var
  Lines: TArray<string>;
  LineTokens: TArray<string>;
  LineURI: TArray<string>;
begin
  Result := '';

  if Buffer.IsEmpty then
    Exit;

  Lines := Buffer.Split([#13#10]);
  LineTokens := Lines[0].Split([': ']);
  if (Length(LineTokens) = 1) then
  begin
    LineURI := LineTokens[0].Split([' ']);
    Result := LineURI[1].TrimLeft(['/']).TrimRight(['/']);
  end;
end;

end.
