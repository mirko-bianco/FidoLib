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

unit Fido.JSON.Mapping;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.SysUtils,
  System.DateUtils,
  System.Classes,
  System.JSON,
  System.Hash,
  System.Generics.Defaults,

  Spring,
  Spring.Collections,

  Fido.Json.Utilities;

type
  TJSONMarhallingFromFunc = reference to function(const Value: TValue): string;
  TJSONUnmarhallingToFunc = reference to function(const Value: string; const TypInfo: pTypeInfo): TValue;

  TJSONConvertingFromFunc<T> = reference to function(const Value: T): string;
  TJSONConvertingToFunc<T> = reference to function(const Value: string): T;

  TJSONMarshallingMapping = record
  strict private
    FFrom: TJSONMarhallingFromFunc;
    FTo: TJSONUnmarhallingToFunc;
  public
    constructor Create(const From: TJSONMarhallingFromFunc; const &To: TJSONUnmarhallingToFunc);

    property From: TJSONMarhallingFromFunc read FFrom;
    property &To: TJSONUnmarhallingToFunc read FTo;
  end;

  MappingsUtilities = class
  strict private
    class procedure RegisterType<T>(
      const JSONMarhallingFromFunc: TJSONConvertingFromFunc<T>;
      const JSONUnmarhallingToFunc: TJSONConvertingToFunc<T>); static;

    class procedure RegisterNullableType<T>(
      const JSONMarhallingFromFunc: TJSONConvertingFromFunc<T>;
      const JSONUnmarhallingToFunc: TJSONConvertingToFunc<T>); static;
  public
    class procedure RegisterPrimitive<T>(
      const JSONMarhallingFromFunc: TJSONConvertingFromFunc<T>;
      const JSONUnmarhallingToFunc: TJSONConvertingToFunc<T>); static;

    class procedure RegisterEnumeratives(
      const JSONMarhallingFromFunc: TJSONMarhallingFromFunc;
      const JSONUnmarhallingToFunc: TJSONUnmarhallingToFunc); static;

    class function TryGetPrimitiveType(const TypInfo: pTypeInfo; out Mapping: TJSONMarshallingMapping): boolean; static;
    class function GetEnumeratives: TJSONMarshallingMapping; static;
  end;

procedure RegisterJSONMapping(const TypInfo: pTypeInfo; const From: TJSONMarhallingFromFunc; const &To: TJSONUnmarhallingToFunc);

implementation

var
  JSONMappings: IDictionary<pTypeInfo, TJSONMarshallingMapping>;
  JSONEnumerativesMapping: TJSONMarshallingMapping;

procedure RegisterJSONMapping(const TypInfo: pTypeInfo; const From: TJSONMarhallingFromFunc; const &To: TJSONUnmarhallingToFunc);
begin
  JSONMappings.AddOrSetValue(TypInfo, TJSONMarshallingMapping.Create(From, &To));
end;

{ TJSONMarshallingMapping }

constructor TJSONMarshallingMapping.Create(
  const From: TJSONMarhallingFromFunc;
  const &To: TJSONUnmarhallingToFunc);
begin
  FFrom := From;
  FTo := &To;
end;

{ MappingsUtilities }

class function MappingsUtilities.GetEnumeratives: TJSONMarshallingMapping;
begin
  Result := JSONEnumerativesMapping;
end;

class procedure MappingsUtilities.RegisterEnumeratives(
  const JSONMarhallingFromFunc: TJSONMarhallingFromFunc;
  const JSONUnmarhallingToFunc: TJSONUnmarhallingToFunc);
begin
  JSONEnumerativesMapping := TJSONMarshallingMapping.Create(JSONMarhallingFromFunc, JSONUnmarhallingToFunc);
end;

class procedure MappingsUtilities.RegisterNullableType<T>(
  const JSONMarhallingFromFunc: TJSONConvertingFromFunc<T>;
  const JSONUnmarhallingToFunc: TJSONConvertingToFunc<T>);
begin
  RegisterJSONMapping(
    TypeInfo(Nullable<T>),
    function(const Value: TValue): string
    var
      JSONNull: Shared<TJSONNull>;
    begin
      JSONNull := TJSONNull.Create;

      if Value.AsType<Nullable<T>>.HasValue then
        Result := JSONMarhallingFromFunc(Value.AsType<Nullable<T>>)
      else
        Result := JSONNull.Value.ToJSON;
    end,
    function(const Value: string; const TypInfo: pTypeInfo): TValue
    var
      JSONNull: Shared<TJSONNull>;
    begin
      JSONNull := TJSONNull.Create;

      if Value.IsEmpty or Value.ToLower.Equals(JSONNull.Value.ToJSON.ToLower) then
        Result := TValue.From<Nullable<T>>(nil)
      else
        Result := TValue.From<Nullable<T>>(Nullable<T>.Create(JSONUnmarhallingToFunc(Value)));
    end);
end;

class procedure MappingsUtilities.RegisterPrimitive<T>(
  const JSONMarhallingFromFunc: TJSONConvertingFromFunc<T>;
  const JSONUnmarhallingToFunc: TJSONConvertingToFunc<T>);
begin
  MappingsUtilities.RegisterType<T>(JSONMarhallingFromFunc, JSONUnmarhallingToFunc);
  MappingsUtilities.RegisterNullableType<T>(JSONMarhallingFromFunc, JSONUnmarhallingToFunc);
end;

class procedure MappingsUtilities.RegisterType<T>(
  const JSONMarhallingFromFunc: TJSONConvertingFromFunc<T>;
  const JSONUnmarhallingToFunc: TJSONConvertingToFunc<T>);
begin
  RegisterJSONMapping(
    TypeInfo(T),
    function(const Value: TValue): string
    begin
      Result := JSONMarhallingFromFunc(Value.AsType<T>);
    end,
    function(const Value: string; const TypInfo: pTypeInfo): TValue
    begin
      Result := TValue.From<T>(JSONUnmarhallingToFunc(Value));
    end);
end;

class function MappingsUtilities.TryGetPrimitiveType(
  const TypInfo: pTypeInfo;
  out Mapping: TJSONMarshallingMapping): boolean;
begin
  Result := JSONMappings.TryGetValue(TypInfo, Mapping)
end;

initialization
  JSONMappings := TCollections.CreateDictionary<pTypeInfo, TJSONMarshallingMapping>(
    TEqualityComparer<pTypeInfo>.Construct(
      function(const Left, Right: pTypeInfo): Boolean
      begin
        Result := SameText(Left.RttiType.QualifiedName, Right.RttiType.QualifiedName);
      end,
      function(const Value: pTypeInfo): Integer
      var
        S: string;
      begin
        S := AnsiLowerCase(Value.RttiType.QualifiedName);
        Result := THashBobJenkins.GetHashValue(PChar(S)^, SizeOf(Char) * Length(S), 0);
      end)
  );

  //Register the default enumeratives mapping
  MappingsUtilities.RegisterEnumeratives(
    function(const Value: TValue): string
    var
      JSONNumber: Shared<TJSONNumber>;
    begin
      JSONNumber := TJSONNumber.Create(Integer(Value.AsVariant));
      Result := JSONNumber.Value.ToJSON;
    end,
    function(const Value: string; const TypInfo: pTypeInfo): TValue
    begin
      TValue.Make(StrToInt(Value), TypInfo, Result);
    end);

  //Register the default supported types
  MappingsUtilities.RegisterPrimitive<string>(
    function(const Value: string): string
    begin
      Result := Value;
    end,
    function(const Value: string): string
    begin
      Result := Value;
    end);

  MappingsUtilities.RegisterPrimitive<Int64>(
    function(const Value: Int64): string
    begin
      Result := IntToStr(Value)
    end,
    function(const Value: string): Int64
    var
      Int64Value: Int64;
    begin
      Result := 0;
      if TryStrToInt64(Value, Int64Value) then
        Result := Int64Value
    end);

  MappingsUtilities.RegisterPrimitive<Integer>(
    function(const Value: Integer): string
    begin
      Result := IntToStr(Value)
    end,
    function(const Value: string): Integer
    var
      IntegerValue: Integer;
    begin
      Result := 0;
      if TryStrToInt(Value, IntegerValue) then
        Result := IntegerValue
    end);

  MappingsUtilities.RegisterPrimitive<Smallint>(
    function(const Value: Smallint): string
    begin
      Result := IntToStr(Value);
    end,
    function(const Value: string): Smallint
    var
      SmallintValue: Integer;
    begin
      Result := 0;
      if TryStrToInt(Value, SmallintValue) then
        Result := SmallintValue
    end);

  MappingsUtilities.RegisterPrimitive<TDateTime>(
    function(const Value: TDateTime): string
    begin
      Result := DateToISO8601(Value);
    end,
    function(const Value: string): TDateTime
    var
      DateTimeValue: TDateTime;
    begin
      Result := 0;
      if TryISO8601ToDate(Value, DateTimeValue) then
        Result := DateTimeValue
    end);

  MappingsUtilities.RegisterPrimitive<Extended>(
    function(const Value: Extended): string
    var
      FormatSettings: TFormatSettings;
    begin
      FormatSettings := TFormatSettings.Create;
      FormatSettings.ThousandSeparator := ',';
      FormatSettings.DecimalSeparator := '.';

      Result := FloatToStr(Value, FormatSettings);
    end,
    function(const Value: string): Extended
    var
      ExtendedValue: Extended;
      FormatSettings: TFormatSettings;
    begin
      FormatSettings := TFormatSettings.Create;
      FormatSettings.ThousandSeparator := ',';
      FormatSettings.DecimalSeparator := '.';
      Result := 0;
      if TryStrToFloat(Value, ExtendedValue, FormatSettings) then
        Result := ExtendedValue
    end);

  MappingsUtilities.RegisterPrimitive<Double>(
    function(const Value: Double): string
    var
      FormatSettings: TFormatSettings;
    begin
      FormatSettings := TFormatSettings.Create;
      FormatSettings.ThousandSeparator := ',';
      FormatSettings.DecimalSeparator := '.';

      Result := FloatToStr(Value, FormatSettings);
    end,
    function(const Value: string): Double
    var
      DoubleValue: Double;
      FormatSettings: TFormatSettings;
    begin
      FormatSettings := TFormatSettings.Create;
      FormatSettings.ThousandSeparator := ',';
      FormatSettings.DecimalSeparator := '.';
      Result := 0;
      if TryStrToFloat(Value, DoubleValue, FormatSettings) then
        Result := DoubleValue
    end);

  MappingsUtilities.RegisterPrimitive<Currency>(
    function(const Value: Currency): string
    var
      FormatSettings: TFormatSettings;
    begin
      FormatSettings := TFormatSettings.Create;
      FormatSettings.ThousandSeparator := ',';
      FormatSettings.DecimalSeparator := '.';

      Result := CurrToStr(Value, FormatSettings);
    end,
    function(const Value: string): Currency
    var
      CurrencyValue: Currency;
      FormatSettings: TFormatSettings;
    begin
      FormatSettings := TFormatSettings.Create;
      FormatSettings.ThousandSeparator := ',';
      FormatSettings.DecimalSeparator := '.';
      Result := 0;
      if TryStrToCurr(Value, CurrencyValue, FormatSettings) then
        Result := CurrencyValue
    end);

  MappingsUtilities.RegisterPrimitive<Boolean>(
    function(const Value: Boolean): string
    var
      JSONBool: Shared<TJSONBool>;
    begin
      JSONBool := TJSONBool.Create(Value);
      Result := JSONBool.Value.ToJSON;
    end,
    function(const Value: string): Boolean
    var
      JSONTrue: Shared<TJSONTrue>;
    begin
      JSONTrue := TJSONTrue.Create;

      Result := False;
      if SameText(Value, JSONTrue.Value.ToJSON) then
        Result := True;
    end);

  MappingsUtilities.RegisterPrimitive<TGuid>(
    function(const Value: TGuid): string
    begin
      Result := StringReplace(StringReplace(GUIDToString(Value), '{', '', []), '}', '', [])
    end,
    function(const Value: string): TGuid
    var
      GuidValue: TGuid;
    begin
      Result := TGuid.Empty;
      if JsonUtilities.TryStringToTGuid(Value, GuidValue) then
        Result := GuidValue;
    end);
end.
