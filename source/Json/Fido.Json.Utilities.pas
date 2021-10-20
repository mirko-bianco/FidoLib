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
{$I Jedi.inc}
unit Fido.Json.Utilities;

interface

uses
  System.JSON,
  System.SysUtils;

type

  TJSONUnQuotedString = class(TJSONString)
  public
    function EstimatedByteSize: Integer; override;
    {$IFNDEF DELPHIX_RIO_UP}
    function ToBytes(const Data: TArray<Byte>; const Idx: Integer): Integer; override;
    {$ELSEIF DELPHIX_RIO_UP}
    function ToBytes(const Data: TArray<Byte>; Offset: Integer): Integer; override;
    procedure ToChars(Builder: TStringBuilder; Options: TJSONAncestor.TJSONOutputOptions); override;
    {$ENDIF}

  end;

  JsonUtilities = record
    class function TryStringToTGuid(const Input: string; out Guid: TGuid): Boolean; static;
  end;

implementation

function IncrAfter(var Arg: Integer): Integer; inline;
begin
  Result := Arg;
  Inc(Arg);
end;

class function JsonUtilities.TryStringToTGuid(
  const Input: string;
  out Guid: TGuid): Boolean;
begin
  Result := False;
  try
    Guid := StringToGuid(Format('{%s}', [Input]));
    Result := True;
  except
  end;
end;

{ TJSONUnQuotedString }

function TJSONUnQuotedString.EstimatedByteSize: Integer;
begin
  if FIsNull then
    Result := 4
  else
    Result := 6 * System.Length(FValue);
end;

{$IFNDEF DELPHIX_RIO_UP}
function TJSONUnQuotedString.EstimatedByteSize: Integer;
begin
  if IsNull then
    Result := 4
  else
    Result := 6 * System.Length(Value);
end;

function TJSONUnQuotedString.ToBytes(const Data: TArray<Byte>; const Idx: Integer): Integer;
var
  Offset: Integer;
  Index: Integer;
  Count: Integer;
  CurrentChar: WideChar;
  UnicodeValue: Integer;
begin
  Offset := Idx;
  if Null then
  begin
    Data[IncrAfter(Offset)] := Ord('n');
    Data[IncrAfter(Offset)] := Ord('u');
    Data[IncrAfter(Offset)] := Ord('l');
    Data[IncrAfter(Offset)] := Ord('l');
  end
  else
  begin
    Index := 0;
    Count := FStrBuffer.Length;
    while Index < Count do
    begin
      CurrentChar := FStrBuffer.Chars[IncrAfter(Index)];
      case CurrentChar of
        '"':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('"');
          end;
        '\':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('\');
          end;
        '/':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('/');
          end;
        #$8:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('b');
          end;
        #$c:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('f');
          end;
        #$a:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('n');
          end;
        #$d:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('r');
          end;
        #$9:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('t');
          end;
        else
          if (CurrentChar < WideChar(32)) or (CurrentChar > WideChar(127)) then
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('u');
            UnicodeValue := Ord(CurrentChar);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 61440) shr 12);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 3840) shr 8);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 240) shr 4);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 15));
          end
          else
            Data[IncrAfter(Offset)] := Ord(CurrentChar);
      end;
    end;
  end;
  Result := Offset;
end;
{$ELSEIF DELPHIX_RIO_UP}
function TJSONUnQuotedString.ToBytes(
  const Data: TArray<Byte>;
  Offset: Integer): Integer;
var
  CurrentChar: Char;
  UnicodeValue: Integer;
begin
  if FIsNull then
  begin
    Data[IncrAfter(Offset)] := Ord('n');
    Data[IncrAfter(Offset)] := Ord('u');
    Data[IncrAfter(Offset)] := Ord('l');
    Data[IncrAfter(Offset)] := Ord('l');
  end
  else
  begin
    for CurrentChar in FValue do
    begin
      case CurrentChar of
        '"':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('"');
          end;
        '\':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('\');
          end;
        '/':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('/');
          end;
        #$8:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('b');
          end;
        #$c:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('f');
          end;
        #$a:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('n');
          end;
        #$d:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('r');
          end;
        #$9:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('t');
          end;
        else
          if (CurrentChar >= Char(32)) and (CurrentChar <= Char(127)) then
            Data[IncrAfter(Offset)] := Ord(CurrentChar)
          else
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('u');
            UnicodeValue := Ord(CurrentChar);
            Data[IncrAfter(Offset)] := DecimalToHex((UnicodeValue and 61440) shr 12);
            Data[IncrAfter(Offset)] := DecimalToHex((UnicodeValue and 3840) shr 8);
            Data[IncrAfter(Offset)] := DecimalToHex((UnicodeValue and 240) shr 4);
            Data[IncrAfter(Offset)] := DecimalToHex((UnicodeValue and 15));
          end;
      end;
    end;
  end;
  Result := Offset;
end;

procedure TJSONUnQuotedString.ToChars(
 Builder: TStringBuilder;
 Options: TJSONAncestor.TJSONOutputOptions);

  procedure AppendWithSpecialChars(
    Builder: TStringBuilder;
    Options: TJSONAncestor.TJSONOutputOptions);
  var
    P, PEnd: PChar;
    UnicodeValue: Integer;
    Buff: array [0 .. 5] of Char;
  begin
    P := Pointer(FValue);
    PEnd := P + Length(FValue);
    while P < PEnd do
    begin
      case P^ of
      '"': Builder.Append('\"');
      '\': Builder.Append('\\');
      '/': Builder.Append('\/');
      #$8: Builder.Append('\b');
      #$9: Builder.Append('\t');
      #$a: Builder.Append('\n');
      #$c: Builder.Append('\f');
      #$d: Builder.Append('\r');
      #0 .. #7, #$b, #$e .. #31, #$0080 .. High(Char):
        begin
          UnicodeValue := Ord(P^);
          if (TJSONOutputOption.EncodeBelow32 in Options) and (UnicodeValue < 32) or
             (TJSONOutputOption.EncodeAbove127 in Options) and (UnicodeValue > 127) then
          begin
            Buff[0] := '\';
            Buff[1] := 'u';
            Buff[2] := Char(DecimalToHex((UnicodeValue and 61440) shr 12));
            Buff[3] := Char(DecimalToHex((UnicodeValue and 3840) shr 8));
            Buff[4] := Char(DecimalToHex((UnicodeValue and 240) shr 4));
            Buff[5] := Char(DecimalToHex((UnicodeValue and 15)));
            Builder.Append(@Buff[0], 0, High(Buff) + 1);
          end
          else
            Builder.Append(P^);
        end;
      else
        Builder.Append(P^);
      end;
      Inc(P);
    end;
  end;

{$WARNINGS OFF}
  function ContainsSpecialChars: Boolean;
  var
    P, PEnd: PChar;
  begin
    P := Pointer(FValue);
    PEnd := P + Length(FValue);
    while P < PEnd do
    begin
      if P^ in ['"', '\', '/', #$8, #$9, #$a, #$c, #$d] then
        Exit(True);
      Inc(P);
    end;
    Result := False;
  end;

  function ContainsSpecialCharsExt(Options: TJSONOutputOptions): Boolean;
  var
    P, PEnd: PChar;
  begin
    P := Pointer(FValue);
    PEnd := P + Length(FValue);
    while P < PEnd do
    begin
      case P^ of
      '"', '\', '/', #$8, #$9, #$a, #$c, #$d:
        Exit(True);
      #0 .. #7, #$b, #$e .. #31:
        if TJSONOutputOption.EncodeBelow32 in Options then
          Exit(True);
      #$0080 .. High(Char):
        if TJSONOutputOption.EncodeAbove127 in Options then
          Exit(True);
      end;
      Inc(P);
    end;
    Result := False;
  end;
{$WARNINGS ON}

var
  LSpecChars: Boolean;
begin
  if FIsNull then
    Builder.Append('null')
  else
  begin
    if Options <> [] then
      LSpecChars := ContainsSpecialCharsExt(Options)
    else
      LSpecChars := ContainsSpecialChars;
    if LSpecChars then
      AppendWithSpecialChars(Builder, Options)
    else
      Builder.Append(FValue);
  end;
end;
{$ENDIF}

end.
