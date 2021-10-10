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
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  {$ifdef POSIX}
  Posix.Pthread,
  {$endif}
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

  Fido.Exceptions,
  Fido.Json.Utilities;

type
  TJSONMarhallingFromFunc = reference to function(const Value: TValue): Nullable<string>;
  TJSONUnmarhallingToFunc = reference to function(const Value: string; const TypInfo: pTypeInfo): TValue;

  TJSONConvertingFromFunc<T> = reference to function(const Value: T): string;
  TJSONConvertingToFunc<T> = reference to function(const Value: string): T;

  MappingsUtilities = class
  public type
    TJSONMarshallingMapping = record
    strict private
      FFrom: TJSONMarhallingFromFunc;
      FTo: TJSONUnmarhallingToFunc;
    public
      constructor Create(const From: TJSONMarhallingFromFunc; const &To: TJSONUnmarhallingToFunc);

      property From: TJSONMarhallingFromFunc read FFrom;
      property &To: TJSONUnmarhallingToFunc read FTo;
    end;

  private type
    TJsonTypeMappingsDictionary = class
    strict private
      FLock: IReadWriteSync;
      FItems: IDictionary<string, IDictionary<pTypeInfo, TJSONMarshallingMapping>>;
    public
      constructor Create;

      procedure AddOrSet(const TInfo: pTypeInfo; const Mapping: TJSONMarshallingMapping; const ConfigurationName: string);
      function TryGet(const TInfo: pTypeInfo; out Mapping: TJSONMarshallingMapping; const ConfigurationName: string): Boolean;
    end;

    TJsonEnumerativeMappingsDictionary = class
    strict private
      FLock: IReadWriteSync;
      FItems: IDictionary<string, TJSONMarshallingMapping>;
    public
      constructor Create;

      procedure AddOrSet(const Mapping: TJSONMarshallingMapping; const ConfigurationName: string);
      function TryGet(out Mapping: TJSONMarshallingMapping; const ConfigurationName: string): Boolean;
    end;

  public const
    DefaultConfigurationName: string = 'Default';
  strict private
    class procedure RegisterJSONMapping(const TypInfo: pTypeInfo; const From: TJSONMarhallingFromFunc; const &To: TJSONUnmarhallingToFunc; const ConfigurationName: string); static;
    class procedure RegisterType<T>(const JSONMarhallingFromFunc: TJSONConvertingFromFunc<T>; const JSONUnmarhallingToFunc: TJSONConvertingToFunc<T>; const ConfigurationName: string); static;
    class procedure RegisterNullableType<T>(const JSONMarhallingFromFunc: TJSONConvertingFromFunc<T>; const JSONUnmarhallingToFunc: TJSONConvertingToFunc<T>; const ConfigurationName: string); static;
    class function SanitizeConfigurationName(const ConfigurationName: string): string; static;
  public
    class procedure RegisterPrimitive<T>(const JSONMarhallingFromFunc: TJSONConvertingFromFunc<T>; const JSONUnmarhallingToFunc: TJSONConvertingToFunc<T>; const ConfigurationName: string = ''); static;
    class procedure RegisterEnumeratives(const JSONMarhallingFromFunc: TJSONMarhallingFromFunc; const JSONUnmarhallingToFunc: TJSONUnmarhallingToFunc; const ConfigurationName: string = ''); static;
    class function TryGetPrimitiveType(const TypInfo: pTypeInfo; out Mapping: TJSONMarshallingMapping; const ConfigurationName: string): boolean; static;
    class function TryGetEnumeratives(out Mapping: TJSONMarshallingMapping; const ConfigurationName: string): Boolean; static;
  end;

implementation

var
  JSONMappings: MappingsUtilities.TJsonTypeMappingsDictionary;
  JSONEnumerativesMappings: MappingsUtilities.TJsonEnumerativeMappingsDictionary;
  Comparer: TOrdinalIStringComparer;

{ TJsonTypeMappingsDictionary }

constructor MappingsUtilities.TJsonTypeMappingsDictionary.Create;
begin
  inherited Create;
  FLock := TMREWSync.Create;
  FItems := Spring.Collections.TCollections.CreateDictionary<string, IDictionary<pTypeInfo, TJSONMarshallingMapping>>(Comparer);
end;

function MappingsUtilities.TJsonTypeMappingsDictionary.TryGet(
  const TInfo: pTypeInfo;
  out Mapping: TJSONMarshallingMapping;
  const ConfigurationName: string): Boolean;
var
  Mappings: IDictionary<pTypeInfo, TJSONMarshallingMapping>;
  Found: Boolean;
begin
  FLock.BeginRead;
  try
     Found := FItems.TryGetValue(ConfigurationName, Mappings);
  finally
    FLock.EndRead;
  end;
  if not Found then
    Exit(False);

  FLock.BeginRead;
  try
     Result := Mappings.TryGetValue(TInfo, Mapping);
  finally
    FLock.EndRead;
  end;
end;

procedure MappingsUtilities.TJsonTypeMappingsDictionary.AddOrSet(
  const TInfo: pTypeInfo;
  const Mapping: TJSONMarshallingMapping;
  const ConfigurationName: string);
var
  Mappings: IDictionary<pTypeInfo, TJSONMarshallingMapping>;
  Found: Boolean;
begin
  FLock.BeginWrite;
  try
    Found := FItems.TryGetValue(ConfigurationName, Mappings);
    if not Found then
    begin
      Mappings := Spring.Collections.TCollections.CreateDictionary<pTypeInfo, TJSONMarshallingMapping>(
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
          end));

      FItems[ConfigurationName] := Mappings;
    end;
    Mappings[TInfo] := Mapping;
  finally
    FLock.EndWrite;
  end;
end;

{ TJsonEnumerativeMappingsDictionary }

procedure MappingsUtilities.TJsonEnumerativeMappingsDictionary.AddOrSet(
  const Mapping: TJSONMarshallingMapping;
  const ConfigurationName: string);
begin
  FLock.BeginWrite;
  try
    FItems[ConfigurationName] := Mapping;
  finally
    FLock.EndWrite;
  end;
end;

constructor MappingsUtilities.TJsonEnumerativeMappingsDictionary.Create;
begin
  inherited Create;
  FLock := TMREWSync.Create;
  FItems := Spring.Collections.TCollections.CreateDictionary<string, TJSONMarshallingMapping>(Comparer);
end;

function MappingsUtilities.TJsonEnumerativeMappingsDictionary.TryGet(
  out Mapping: TJSONMarshallingMapping;
  const ConfigurationName: string): Boolean;
begin
  FLock.BeginRead;
  try
     Result := FItems.TryGetValue(ConfigurationName, Mapping);
  finally
    FLock.EndRead;
  end;
end;

{ TJSONMarshallingMapping }

constructor MappingsUtilities.TJSONMarshallingMapping.Create(
  const From: TJSONMarhallingFromFunc;
  const &To: TJSONUnmarhallingToFunc);
begin
  FFrom := From;
  FTo := &To;
end;

{ MappingsUtilities }

class procedure MappingsUtilities.RegisterJSONMapping(
  const TypInfo: pTypeInfo;
  const From: TJSONMarhallingFromFunc;
  const &To: TJSONUnmarhallingToFunc;
  const ConfigurationName: string);
begin
  JSONMappings.AddOrSet(TypInfo, TJSONMarshallingMapping.Create(From, &To), ConfigurationName);
end;

class function MappingsUtilities.TryGetEnumeratives(
  out Mapping: TJSONMarshallingMapping;
  const ConfigurationName: string): Boolean;
var
  Config: string;
begin
  Config := SanitizeConfigurationName(ConfigurationName);
  Result := JSONEnumerativesMappings.TryGet(Mapping, Config);
  if not Result and not Config.ToLower.Equals(MappingsUtilities.DefaultConfigurationName.ToLower) then
    Result := JSONEnumerativesMappings.TryGet(Mapping, MappingsUtilities.DefaultConfigurationName);
end;

class procedure MappingsUtilities.RegisterEnumeratives(
  const JSONMarhallingFromFunc: TJSONMarhallingFromFunc;
  const JSONUnmarhallingToFunc: TJSONUnmarhallingToFunc;
  const ConfigurationName: string);
begin
  JSONEnumerativesMappings.AddOrSet(TJSONMarshallingMapping.Create(JSONMarhallingFromFunc, JSONUnmarhallingToFunc), SanitizeConfigurationName(ConfigurationName));
end;

class procedure MappingsUtilities.RegisterNullableType<T>(
  const JSONMarhallingFromFunc: TJSONConvertingFromFunc<T>;
  const JSONUnmarhallingToFunc: TJSONConvertingToFunc<T>;
  const ConfigurationName: string);
begin
  RegisterJSONMapping(
    TypeInfo(Nullable<T>),
    function(const Value: TValue): Nullable<string>
    begin
      if Value.AsType<Nullable<T>>.HasValue then
        Result := JSONMarhallingFromFunc(Value.AsType<Nullable<T>>);
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
    end,
    ConfigurationName);
end;

class procedure MappingsUtilities.RegisterPrimitive<T>(
  const JSONMarhallingFromFunc: TJSONConvertingFromFunc<T>;
  const JSONUnmarhallingToFunc: TJSONConvertingToFunc<T>;
  const ConfigurationName: string);
begin
  MappingsUtilities.RegisterType<T>(JSONMarhallingFromFunc, JSONUnmarhallingToFunc, SanitizeConfigurationName(ConfigurationName));
  MappingsUtilities.RegisterNullableType<T>(JSONMarhallingFromFunc, JSONUnmarhallingToFunc, SanitizeConfigurationName(ConfigurationName));
end;

class procedure MappingsUtilities.RegisterType<T>(
  const JSONMarhallingFromFunc: TJSONConvertingFromFunc<T>;
  const JSONUnmarhallingToFunc: TJSONConvertingToFunc<T>;
  const ConfigurationName: string);
var
  Config: string;
begin
  Config := ConfigurationName;
  if Config.IsEmpty then
    Config := DefaultConfigurationName;

  RegisterJSONMapping(
    TypeInfo(T),
    function(const Value: TValue): Nullable<string>
    begin
      Result := JSONMarhallingFromFunc(Value.AsType<T>);
    end,
    function(const Value: string; const TypInfo: pTypeInfo): TValue
    begin
      Result := TValue.From<T>(JSONUnmarhallingToFunc(Value));
    end,
    ConfigurationName);
end;

class function MappingsUtilities.SanitizeConfigurationName(const ConfigurationName: string): string;
begin
  Result := ConfigurationName;
  if Result.IsEmpty then
    Result := DefaultConfigurationName;
end;

class function MappingsUtilities.TryGetPrimitiveType(
  const TypInfo: pTypeInfo;
  out Mapping: TJSONMarshallingMapping;
  const ConfigurationName: string): boolean;
var
  Config: string;
begin
  Config := SanitizeConfigurationName(ConfigurationName);
  Result := JSONMappings.TryGet(TypInfo, Mapping, Config);
  if not Result and not Config.ToLower.Equals(MappingsUtilities.DefaultConfigurationName.ToLower) then
    Result := JSONMappings.TryGet(TypInfo, Mapping, MappingsUtilities.DefaultConfigurationName);
end;

initialization
  Comparer := TOrdinalIStringComparer.Create;
  JSONMappings := MappingsUtilities.TJsonTypeMappingsDictionary.Create;
  JSONEnumerativesMappings := MappingsUtilities.TJsonEnumerativeMappingsDictionary.Create;

  //Register the default enumeratives mapping
  MappingsUtilities.RegisterEnumeratives(
    function(const Value: TValue): Nullable<string>
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

finalization
  JSONMappings.Free;
  JSONEnumerativesMappings.Free;
  Comparer.Free;
end.
