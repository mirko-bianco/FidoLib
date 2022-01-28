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
  Fido.Exceptions,
  Fido.JSON.Mapping;

type
  EEventsDrivenUtilities = class(EFidoException);

  TMappingMethod = reference to function(const Payload: TValue; const ParamsCount: Integer; const Method: TRttiMethod): TArray<TValue>;

  TEventsDrivenUtilities = record
  private class var
    FMappings: IDictionary<Pointer, TMappingMethod>;
  public
    class constructor Create;
    class function FormatKey(const Channel: string; const EventName: string): string; static;
    class function PayloadToMethodParams<PayloadType>(const Payload: PayloadType; const Method: TRttiMethod): TArray<TValue>; static;

    class procedure RegisterPayloadTypeMapper<PayloadType>(const Method: TMappingMethod); static;
  end;

implementation

{ TEventsDrivenUtilities }

class constructor TEventsDrivenUtilities.Create;
begin
  FMappings := TCollections.CreateDictionary<Pointer, TMappingMethod>;

  FMappings.Items[TypeInfo(string)] := function(const Payload: TValue; const ParamsCount: Integer; const Method: TRttiMethod): TArray<TValue>
    var
      JSONValue: Shared<TJSONValue>;
      Index: Integer;
    begin
      SetLength(Result, ParamsCount);

      JSONValue := TJSONValue.ParseJSONValue(Payload.AsType<string>);

      if ParamsCount = 1 then
      begin
        if JSONValue.Value is TJSONArray then
          Result[0] := JSONUnmarshaller.To(TJSONArray(JSONValue.Value).Items[0].ToJSON, Method.GetParameters[0].ParamType.Handle)
        else
          Result[0] := JSONUnmarshaller.To(Payload.AsType<string>, Method.GetParameters[0].ParamType.Handle);
        Exit(Result);
      end;

      for Index := 0 to Min(TJSONArray(JSONValue.Value).Count, ParamsCount) - 1 do
        Result[Index] := JSONUnmarshaller.To(TJSONArray(JSONValue.Value).Items[Index].ToJSON, Method.GetParameters[Index].ParamType.Handle);
    end;

  FMappings.Items[TypeInfo(TArray<TValue>)] := function(const Payload: TValue; const ParamsCount: Integer; const Method: TRttiMethod): TArray<TValue>
    var
      Index: Integer;
    begin
      SetLength(Result, ParamsCount);

      for Index := 0 to Min(Length(Payload.AsType<TArray<TValue>>), ParamsCount) - 1 do
        Result[Index] := Payload.AsType<TArray<TValue>>[Index];
    end;
end;

class function TEventsDrivenUtilities.FormatKey(
  const Channel: string;
  const EventName: string): string;
begin
   Result := Format('%s::%s', [Channel, EventName]);
end;

class function TEventsDrivenUtilities.PayloadToMethodParams<PayloadType>(const Payload: PayloadType; const Method: TRttiMethod): TArray<TValue>;
var
  ParametersNo: integer;
  MappingMethod: TMappingMethod;
  Ctx: TRttiContext;
  PayloadTypeTypeInfo: Pointer;
begin
  Result := [];

  ParametersNo := Length(Method.GetParameters);
  if ParametersNo = 0 then
    Exit(Result);

  Ctx := TRttiContext.Create;

  PayloadTypeTypeInfo := TypeInfo(PayloadType);

  if not FMappings.TryGetValue(PayloadTypeTypeInfo, MappingMethod) then
    raise EEventsDrivenUtilities.CreateFmt('TEventsDrivenUtilities.PayloadToMethodParams: Type %s not supported.', [Ctx.GetType(PayloadTypeTypeInfo).QualifiedName]);

  Result := MappingMethod(TValue.From<PayloadType>(Payload), ParametersNo, Method);
end;

class procedure TEventsDrivenUtilities.RegisterPayloadTypeMapper<PayloadType>(const Method: TMappingMethod);
begin
  FMappings.Items[TypeInfo(PayloadType)] := Method;
end;

end.
