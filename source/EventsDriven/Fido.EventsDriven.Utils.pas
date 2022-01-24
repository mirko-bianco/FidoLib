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

unit Fido.EventsDriven.Utils;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections,
  System.JSON,
  System.Rtti,
  System.Math,

  Spring,
  Spring.Collections,

  Fido.JSON.Marshalling,
  Fido.JSON.Mapping;

type
  TEventsDrivenUtilities = record
    class function FormatKey(const Channel: string; const EventName: string): string; static;
    class function PayloadToMethodParams(const Payload: string; const Method: TRttiMethod): TArray<TValue>; static;
  end;

implementation

{ TEventsDrivenUtilities }

class function TEventsDrivenUtilities.FormatKey(
  const Channel: string;
  const EventName: string): string;
begin
   Result := Format('%s::%s', [Channel, EventName]);
end;

class function TEventsDrivenUtilities.PayloadToMethodParams(const Payload: string; const Method: TRttiMethod): TArray<TValue>;
var
  JSONValue: Shared<TJSONValue>;
  ParametersNo: integer;
  Index: Integer;
begin
  Result := [];

  ParametersNo := Length(Method.GetParameters);
  if ParametersNo = 0 then
    Exit(Result);

  SetLength(Result, ParametersNo);

  JSONValue := TJSONValue.ParseJSONValue(Payload);

  if ParametersNo = 1 then
  begin
    if JSONValue.Value is TJSONArray then
      Result[0] := JSONUnmarshaller.To(TJSONArray(JSONValue.Value).Items[0].ToJSON, Method.GetParameters[0].ParamType.Handle)
    else
      Result[0] := JSONUnmarshaller.To(Payload, Method.GetParameters[0].ParamType.Handle);
    Exit(Result);
  end;

  for Index := 0 to Min(TJSONArray(JSONValue.Value).Count, ParametersNo) - 1 do
    Result[Index] := JSONUnmarshaller.To(TJSONArray(JSONValue.Value).Items[Index].ToJSON, Method.GetParameters[Index].ParamType.Handle);

end;
end.
