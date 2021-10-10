(*             * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
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

unit Fido.Testing.Mock.Utils;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  System.DateUtils,
  System.Math,

  Spring,
  Spring.Mocking,

  Fido.Types,
  Fido.Exceptions;

type
  EFidoMockUtilsException = class(EFidoException);

  MockUtils = class;

  TClassOfMockUtils = class of MockUtils;

  MockUtils = class
  strict private class var
    FNullChance: Extended;
    FStringPrefix: string;
    FStringLengthLow: Integer;
    FStringLengthHigh: Integer;
    FLowInteger: Integer;
    FHighInteger: Integer;
    FLowExtended: Extended;
    FHighExtended: Extended;
    FMaxDecimalDigits: Integer;
    FLowDateTime: TDateTime;
    FHighDateTime: TDateTime;
    FLowDate: TDate;
    FHighDate: TDate;
    FLowTime: TTime;
    FHighTime: TTime;
    FLowInt64: Int64;
    FHighInt64: Int64;
    FLowNativeInt: NativeInt;
    FHighNativeInt: NativeInt;
    FLowSmallint: SmallInt;
    FHighSmallint: SmallInt;
  strict private
    class function RandomBoolean: boolean;
    class function RandomNull(const NullChance: Extended): boolean;
    class function CleanGuidString: string;
    class function SomeEnumValue<T: record>(TypeInfo: PTypeInfo): T; overload;
    class function SomeEnumValue(TypeInfo: PTypeInfo): TValue; overload;
    class procedure SetupSomeObject<T: class>(AObject: TObject);
  private
    class procedure Initialize;
  public
    class function SomeInteger: Integer;
    class function SomeString: string;
    class function SomeExtended: Double;
    class function SomeBoolean: boolean;
    class function SomeDateTime: TDateTime;
    class function SomeDate: TDate;
    class function SomeTime: TTime;
    class function SomeInt64: Int64;
    class function SomeNativeInt: NativeInt;
    class function SomeGuid: TGuid;
    class function SomeSmallInt: SmallInt;

    class function SomeNullableInteger: TNullableInteger;
    class function SomeNullableString: TNullableString;
    class function SomeNullableExtended: TNullableDouble;
    class function SomeNullableBoolean: TNullableBoolean;
    class function SomeNullableDateTime: TNullableDateTime;
    class function SomeNullableInt64: TNullableInt64;
    class function SomeNullableNativeInt: TNullableNativeInt;
    class function SomeNullableGuid: TNullableGuid;
    class function SomeNullableSmallint: TNullableSmallint;

    class function SomeEnum<T: record>: T;

    class function MockSomeObject<T: class, constructor>: Mock<T>; overload;
    class function SomeObject<T: class, constructor>: T; overload;

    class function WithNullChance(const NullChance: Extended): TClassOfMockUtils;

    class function WithStringPrefix(const StringPrefix: string): TClassOfMockUtils;
    class function WithStringLength(const LowLength, HighLength: Integer): TClassOfMockUtils;

    class function WithIntegerRange(const LowInteger, HighInteger: Integer): TClassOfMockUtils;
    class function WithExtendedRange(const LowExtended, HighExtended: Double): TClassOfMockUtils;
    class function WithDateTimeRange(const LowDateTime, HighDateTime: TDateTime): TClassOfMockUtils;
    class function WithDateRange(const LowDate, HighDate: TDate): TClassOfMockUtils;
    class function WithTimeRange(const LowTime, HighTime: TTime): TClassOfMockUtils;
    class function WithInt64Range(const LowInt64, HighInt64: Int64): TClassOfMockUtils;
    class function WithNativeIntRange(const LowNativeInt, HighNativeInt: NativeInt): TClassOfMockUtils;
    class function WithSmallintRange(const LowSmallint, HighSmallint: Smallint): TClassOfMockUtils;

    class function WithIntegerFrom(const LowInteger: Integer): TClassOfMockUtils;
    class function WithExtendedFrom(const LowExtended: Double): TClassOfMockUtils;
    class function WithDateTimeFrom(const LowDateTime: TDateTime): TClassOfMockUtils;
    class function WithDateFrom(const LowDate: TDate): TClassOfMockUtils;
    class function WithTimeFrom(const LowTime: TTime): TClassOfMockUtils;
    class function WithInt64From(const LowInt64: Int64): TClassOfMockUtils;
    class function WithNativeIntFrom(const LowNativeInt: NativeInt): TClassOfMockUtils;

    class function WithExtendedDecimalsUpTo(const MaxDecimalDigits: Integer): TClassOfMockUtils;

    class function WithRanges(const LowInteger, HighInteger: Integer; const LowExtended, HighExtended: Extended; const LowDateTime, HighDateTime: TDateTime; const LowDate, HighDate: TDate;
      const LowTime, HighTime: TTime; const LowInt64, HighInt64: Int64; const LowNativeInt, HighNativeInt: NativeInt): TClassOfMockUtils;

  end;

implementation

class function MockUtils.RandomBoolean: boolean;
begin
  Result := (Random < 0.5);
end;

class function MockUtils.RandomNull(const NullChance: Extended): boolean;
begin
  Result := (Random < NullChance);
end;

class function MockUtils.CleanGuidString: string;
begin
  Result := Copy(StringReplace(TGuid.NewGuid.ToString, '-', '', [rfReplaceAll]), 2, 32);
end;

class function MockUtils.SomeEnumValue<T>(TypeInfo: PTypeInfo): T;
var
  EnumerationType: TRttiEnumerationType;
  Value: Integer;
begin
  EnumerationType := TypeInfo.RttiType as TRttiEnumerationType;
  Value := Random(EnumerationType.MaxValue);

  case GetTypeData(TypeInfo)^.OrdType of
    otUByte, otSByte:
      PByte(@Result)^ := Value;
    otUWord, otSWord:
      PWord(@Result)^ := Value;
    otULong, otSLong:
      PInteger(@Result)^ := Value;
  end;
end;

class function MockUtils.SomeEnumValue(TypeInfo: PTypeInfo): TValue;
var
  EnumerationType: TRttiEnumerationType;
  Value: TValue;
begin
  EnumerationType := TypeInfo.RttiType as TRttiEnumerationType;
  Value := TValue.FromOrdinal(TypeInfo, Random(EnumerationType.MaxValue + 1));
end;

class function MockUtils.SomeInteger: Integer;
begin
  Result := FLowInteger + Random(FHighInteger - FLowInteger);
  Initialize;
end;

class function MockUtils.SomeSmallInt: SmallInt;
begin
  Result := FLowSmallint + Random(FHighSmallint - FLowSmallint);
  Initialize;
end;

class function MockUtils.SomeString: string;
var
  NewStringLength: Integer;
begin
  if (FStringLengthLow = 0) and (FStringLengthHigh = 0) then
    Exit(FStringPrefix + CleanGuidString);

  NewStringLength := FStringLengthLow + Random(FStringLengthHigh - FStringLengthLow);

  Result := FStringPrefix;
  while Length(Result) < NewStringLength do
    Result := Result + CleanGuidString;

  Result := Copy(Result, 1, NewStringLength);

  Initialize;
end;

class function MockUtils.SomeExtended: Double;
begin
  Result := RoundTo(FLowExtended + (Random * (FHighExtended - FLowExtended)), -FMaxDecimalDigits);
  Initialize;
end;

class function MockUtils.SomeBoolean: boolean;
begin
  Result := RandomBoolean;
  Initialize;
end;

class function MockUtils.SomeDateTime: TDateTime;
var
  LeadMilliSeconds: Integer;
  TrailMilliSeconds: Integer;
  MilliSecondRange: Integer;
  MilliSecondsToAdd: Integer;
begin
  if Trunc(FLowDateTime) = Trunc(FHighDateTime) then
  begin
    MilliSecondRange := MilliSecondsBetween(FLowDateTime, FHighDateTime);
  end
  else
  begin
    LeadMilliSeconds := MilliSecondsBetween(FLowDateTime, Trunc(FLowDateTime + 1));
    TrailMilliSeconds := MilliSecondsBetween(Trunc(FHighDateTime), FHighDateTime);

    MilliSecondRange := LeadMilliSeconds + TrailMilliSeconds;
  end;

  MilliSecondsToAdd := MilliSecondsBetween(FLowDateTime, Trunc(FLowDateTime)) +
                         Random(MilliSecondRange);

  if MilliSecondRange > 0 then
    Result := IncMilliSecond(Trunc(FLowDateTime), MilliSecondsToAdd)
  else
    Result := FLowDateTime;

  Initialize;
end;

class function MockUtils.SomeDate: TDate;
begin
  Result := FLowDate + Random(Trunc(FHighDate - FLowDate));
  Initialize;
end;

class function MockUtils.SomeTime: TTime;
var
  MilliSeconds: Integer;
  MilliSecondsToAdd: Integer;
begin
  MilliSeconds := MilliSecondsBetween(FLowTime, FHighTime);
  MilliSecondsToAdd := Random(MilliSeconds);
  if MilliSecondsToAdd > 0 then
    Result := IncMilliSecond(FLowTime, MilliSecondsToAdd)
  else
    Result := FLowTime;
  Initialize;
end;

class function MockUtils.SomeInt64: Int64;
var
  Int64ToAdd: Int64;
begin
  Result := FLowInt64;
  Int64Rec(Int64ToAdd).Hi := Random(Int64Rec(FHighInt64).Hi - Int64Rec(FLowInt64).Hi);
  Int64Rec(Int64ToAdd).Lo := Random(Int64Rec(FHighInt64).Lo - Int64Rec(FLowInt64).Lo);

  Result := Result + Int64ToAdd;
  Initialize;
end;

class function MockUtils.SomeNativeInt: NativeInt;
begin
  Result := Random(FHighNativeInt - FLowNativeInt);
  Initialize;
end;

class function MockUtils.SomeGuid: TGuid;
begin
  Result := TGuid.NewGuid;
  Initialize;
end;

class function MockUtils.SomeNullableInteger: TNullableInteger;
begin
  if not RandomNull(FNullChance) then
    Result := SomeInteger;
  Initialize;
end;

class function MockUtils.SomeNullableSmallint: TNullableSmallint;
begin
  if not RandomNull(FNullChance) then
    Result := SomeSmallint;
  Initialize;
end;

class function MockUtils.SomeNullableString: TNullableString;
begin
  if not RandomNull(FNullChance) then
    Result := SomeString;
  Initialize;
end;

class function MockUtils.SomeNullableExtended: TNullableDouble;
begin
  if not RandomNull(FNullChance) then
    Result := SomeExtended;
  Initialize;
end;

class function MockUtils.SomeNullableBoolean: TNullableBoolean;
begin
  if not RandomNull(FNullChance) then
    Result := SomeBoolean;
  Initialize;
end;

class function MockUtils.SomeNullableDateTime: TNullableDateTime;
begin
  if not RandomNull(FNullChance) then
    Result := SomeDateTime;
  Initialize;
end;

class function MockUtils.SomeNullableInt64: TNullableInt64;
begin
  if not RandomNull(FNullChance) then
    Result := SomeInt64;
  Initialize;
end;

class function MockUtils.SomeNullableNativeInt: TNullableNativeInt;
begin
  if not RandomNull(FNullChance) then
    Result := SomeNativeInt;
  Initialize;
end;

class function MockUtils.SomeNullableGuid: TNullableGuid;
begin
  if not RandomNull(FNullChance) then
    Result := SomeGuid;
  Initialize;
end;

class function MockUtils.SomeEnum<T>: T;
var
  TypeInfo: PTypeInfo;
  Value: Integer;
begin
  TypeInfo := PTypeInfo(System.TypeInfo(T));
  if not Assigned(TypeInfo) or (TypeInfo.Kind <> tkEnumeration) then
    raise EFidoMockUtilsException.Create(string(PTypeInfo(System.TypeInfo(T)).Name) + ' is not an Enumeration');

  Result := SomeEnumValue<T>(TypeInfo);
  Initialize;
end;

class procedure MockUtils.SetupSomeObject<T>(AObject: TObject);
var
  RttiContext: TRttiContext;
  RttiProperty: TRttiProperty;
  FieldValue: TValue;
  NewValue: TValue;
  ValueInteger: TNullableInteger;
  ValueString: TNullableString;
  ValueExtended: TNullableDouble;
  ValueBoolean: TNullableBoolean;
  ValueDateTime: TNullableDateTime;
  ValueInt64: TNullableInt64;
  ValueGuid: TGuid;
  NullChance: Extended;
  LowInteger: Integer;
  HighInteger: Integer;
  LowExtended: Extended;
  HighExtended: Extended;
  LowDateTime: TDateTime;
  HighDateTime: TDateTime;
  LowDate: TDate;
  HighDate: TDate;
  LowTime: TTime;
  HighTime: TTime;
  LowInt64: Int64;
  HighInt64: Int64;
  LowNativeInt: NativeInt;
  HighNativeInt: NativeInt;
  StringPrefix: string;
  StringLengthLow: Integer;
  StringLengthHigh: Integer;
begin
  NullChance := FNullChance;
  StringPrefix := FStringPrefix;
  StringLengthLow := FStringLengthLow;
  StringLengthHigh := FStringLengthHigh;
  LowInteger := FLowInteger;
  HighInteger := FHighInteger;
  LowExtended := FLowExtended;
  HighExtended := FHighExtended;
  LowDateTime := FLowDateTime;
  HighDateTime := FHighDateTime;
  LowDate := FLowDate;
  HighDate := FHighDate;
  LowTime := FLowTime;
  HighTime := FHighTime;
  LowInt64 := FLowInt64;
  HighInt64 := FHighInt64;
  LowNativeInt := FLowNativeInt;
  HighNativeInt := FHighNativeInt;

  RttiContext := TRttiContext.Create;

  for RttiProperty in RttiContext.GetType(T).GetProperties do
  begin
    if (RttiProperty.IsReadable and RttiProperty.IsWritable) then
    begin
      if RttiProperty.PropertyType.QualifiedName = 'System.TDateTime' then
      begin
        RttiProperty.SetValue(TObject(AObject), SomeDateTime);
        Continue;
      end;

      if RttiProperty.PropertyType.QualifiedName = 'System.TDate' then
      begin
        RttiProperty.SetValue(TObject(AObject), SomeDate);
        Continue;
      end;

      if RttiProperty.PropertyType.QualifiedName = 'System.TTime' then
      begin
        RttiProperty.SetValue(TObject(AObject), SomeTime);
        Continue;
      end;

      case RttiProperty.PropertyType.TypeKind of
        tkInteger:
          RttiProperty.SetValue(TObject(AObject), SomeInteger);
        tkString,
        tkLString,
        tkWString,
        tkUString:
          if SameText(RttiProperty.Name, 'Name') and
             AObject.InheritsFrom(TComponent) then
            RttiProperty.SetValue(TObject(AObject), 'Name' + IntToStr(Random(MaxInt)))
          else
            RttiProperty.SetValue(TObject(AObject), SomeString);
        tkFloat:
          RttiProperty.SetValue(TObject(AObject), SomeExtended);
        tkInt64:
          RttiProperty.SetValue(TObject(AObject), SomeInt64);
        tkEnumeration:
          begin
            FieldValue := RttiProperty.GetValue(TObject(AObject));
            RttiProperty.SetValue(TObject(AObject), SomeEnumValue(FieldValue.TypeInfo));
          end;
        tkRecord:
          begin
            FieldValue := RttiProperty.GetValue(AObject);

            if FieldValue.IsType<TNullableInteger> then
            begin
              ValueInteger := SomeNullableInteger;
              TValue.Make(@ValueInteger, TypeInfo(TNullableInteger), NewValue);
            end
            else if FieldValue.IsType<TNullableInt64> then
            begin
              ValueInt64 := SomeNullableInt64;
              TValue.Make(@ValueInt64, TypeInfo(TNullableInt64), NewValue);
            end
            else if FieldValue.IsType<TNullableString> then
            begin
              ValueString := SomeNullableString;
              TValue.Make(@ValueString, TypeInfo(TNullableString), NewValue);
            end
            else if FieldValue.IsType<TNullableDouble> then
            begin
              ValueExtended := SomeNullableExtended;
              TValue.Make(@ValueExtended, TypeInfo(TNullableDouble), NewValue);
            end
            else if FieldValue.IsType<TNullableBoolean> then
            begin
              ValueBoolean := SomeNullableBoolean;
              TValue.Make(@ValueBoolean, TypeInfo(TNullableBoolean), NewValue);
            end
            else if FieldValue.IsType<TNullableDateTime> then
            begin
              ValueDateTime := SomeNullableDateTime;
              TValue.Make(@ValueDateTime, TypeInfo(TNullableDateTime), NewValue);
            end
            else if FieldValue.IsType<TGUID> then
            begin
              ValueGuid := SomeGuid;
              TValue.Make(@ValueGuid, TypeInfo(TGUID), NewValue);
            end
            else
            begin
              Continue;
            end;

            RttiProperty.SetValue(TObject(AObject), NewValue);
          end;
      end;

      WithNullChance(NullChance);
      WithStringPrefix(StringPrefix);
      WithStringLength(StringLengthLow, StringLengthHigh);
      WithRanges(LowInteger, HighInteger,
        LowExtended, HighExtended,
        LowDateTime, HighDateTime,
        LowDate, HighDate,
        LowTime, HighTime,
        LowInt64, HighInt64,
        LowNativeInt, HighNativeInt);
    end;
  end;
  Initialize;
end;

class function MockUtils.SomeObject<T>: T;
begin
  Result := T.Create;
  SetupSomeObject<T>(Result);
end;

class function MockUtils.MockSomeObject<T>: Mock<T>;
begin
  Result := Mock<T>.Create;
  SetupSomeObject<T>(Result.Instance);
end;

class function MockUtils.WithNullChance(const NullChance: Extended): TClassOfMockUtils;
begin
  FNullChance := NullChance;

  Result := MockUtils;
end;

class function MockUtils.WithStringPrefix(const StringPrefix: string): TClassOfMockUtils;
begin
  FStringPrefix := StringPrefix;

  Result := MockUtils;
end;

class function MockUtils.WithSmallintRange(
  const LowSmallint: Smallint;
  const HighSmallint: Smallint): TClassOfMockUtils;
begin
  if LowSmallint < 0 then
    raise EFidoMockUtilsException.Create('Smallint range invalid, low must be zero or higher');

  if HighSmallint < 0 then
    raise EFidoMockUtilsException.Create('Smallint range invalid, high must be zero or higher');

  if LowSmallint > HighSmallint then
    raise EFidoMockUtilsException.Create('Smallint range invalid, low must be lower or equal to high');

  FLowSmallint := LowSmallint;
  FHighSmallint := HighSmallint;

  Result := MockUtils;
end;

class function MockUtils.WithStringLength(
  const LowLength: Integer;
  const HighLength: Integer): TClassOfMockUtils;
begin
  if LowLength < 0 then
    raise EFidoMockUtilsException.Create('String length range invalid, low must be zero or higher');

  if HighLength < 0 then
    raise EFidoMockUtilsException.Create('String length range invalid, high must be zero or higher');

  if LowLength > HighLength then
    raise EFidoMockUtilsException.Create('String length range invalid, low must be lower or equal to high');

  FStringLengthLow := LowLength;
  FStringLengthHigh := HighLength;

  Result := MockUtils;
end;

class function MockUtils.WithIntegerRange(
  const LowInteger: Integer;
  const HighInteger: Integer): TClassOfMockUtils;
begin
  if LowInteger > HighInteger then
    raise EFidoMockUtilsException.Create('Integer range invalid, low must be lower or equal to high');
  FLowInteger := LowInteger;
  FHighInteger := HighInteger;

  Result := MockUtils;
end;

class function MockUtils.WithExtendedRange(
  const LowExtended: Double;
  const HighExtended: Double): TClassOfMockUtils;
begin
  if LowExtended > HighExtended then
    raise EFidoMockUtilsException.Create('Extended range invalid, low must be lower or equal to high');
  FLowExtended := LowExtended;
  FHighExtended := HighExtended;

  Result := MockUtils;
end;

class function MockUtils.WithDateTimeRange(
  const LowDateTime: TDateTime;
  const HighDateTime: TDateTime): TClassOfMockUtils;
begin
  if LowDateTime > HighDateTime then
    raise EFidoMockUtilsException.Create('DateTime range invalid, low must be lower or equal to high');
  FLowDateTime := LowDateTime;
  FHighDateTime := HighDateTime;

  Result := MockUtils;
end;

class function MockUtils.WithDateRange(
  const LowDate: TDate;
  const HighDate: TDate): TClassOfMockUtils;
begin
  if LowDate > HighDate then
    raise EFidoMockUtilsException.Create('Date range invalid, low must be lower or equal to high');
  FLowDate := LowDate;
  FHighDate := HighDate;

  Result := MockUtils;
end;

class function MockUtils.WithTimeRange(
  const LowTime: TTime;
  const HighTime: TTime): TClassOfMockUtils;
begin
  if LowTime > HighTime then
    raise EFidoMockUtilsException.Create('Time range invalid, low must be lower or equal to high');
  FLowTime := LowTime;
  FHighTime := HighTime;

  Result := MockUtils;
end;

class function MockUtils.WithInt64Range(
  const LowInt64: Int64;
  const HighInt64: Int64): TClassOfMockUtils;
begin
  if LowInt64 > HighInt64 then
    raise EFidoMockUtilsException.Create('Int64 range invalid, low must be lower or equal to high');
  FLowInt64 := LowInt64;
  FHighInt64 := HighInt64;

  Result := MockUtils;
end;

class function MockUtils.WithNativeIntRange(
  const LowNativeInt: NativeInt;
  const HighNativeInt: NativeInt): TClassOfMockUtils;
begin
  if LowNativeInt > HighNativeInt then
    raise EFidoMockUtilsException.Create('NativeInt range invalid, low must be lower or equal to high');
  FLowNativeInt := LowNativeInt;
  FHighNativeInt := HighNativeInt;

  Result := MockUtils;
end;

class function MockUtils.WithIntegerFrom(const LowInteger: Integer): TClassOfMockUtils;
begin
  Result := WithIntegerRange(LowInteger, MaxInt);
end;

class function MockUtils.WithExtendedDecimalsUpTo(const MaxDecimalDigits: Integer): TClassOfMockUtils;
begin
  if MaxDecimalDigits > 20 then
    raise EFidoMockUtilsException.Create('Extended maximum decimal digits invalid; can be up to 20');
  if MaxDecimalDigits < 0 then
    raise EFidoMockUtilsException.Create('Extended maximum decimal digits invalid; can be from 0');

  FMaxDecimalDigits := MaxDecimalDigits;

  Result := MockUtils;
end;

class function MockUtils.WithExtendedFrom(const LowExtended: Double): TClassOfMockUtils;
begin
  Result := WithExtendedRange(LowExtended, MaxInt);
end;

class function MockUtils.WithDateTimeFrom(const LowDateTime: TDateTime): TClassOfMockUtils;
begin
  Result := WithDateTimeRange(LowDateTime, IncYear(Now, 100));
end;

class function MockUtils.WithDateFrom(const LowDate: TDate): TClassOfMockUtils;
begin
  Result := WithDateRange(LowDate, IncYear(Date, 100));
end;

class function MockUtils.WithTimeFrom(const LowTime: TTime): TClassOfMockUtils;
begin
  Result := WithTimeRange(LowTime, IncMilliSecond(Trunc(Date), 999));
end;

class function MockUtils.WithInt64From(const LowInt64: Int64): TClassOfMockUtils;
var
  HighInt64: Int64;
begin
  Int64Rec(HighInt64).Hi := MaxInt;
  Int64Rec(HighInt64).Lo := MaxInt;
  Result := WithInt64Range(LowInt64, HighInt64);
end;

class function MockUtils.WithNativeIntFrom(const LowNativeInt: NativeInt): TClassOfMockUtils;
begin
  Result := WithNativeIntRange(LowNativeInt, MaxInt);
end;

class function MockUtils.WithRanges(
  const LowInteger: Integer;
  const HighInteger: Integer;
  const LowExtended: Extended;
  const HighExtended: Extended;
  const LowDateTime: TDateTime;
  const HighDateTime: TDateTime;
  const LowDate: TDate;
  const HighDate: TDate;
  const LowTime: TTime;
  const HighTime: TTime;
  const LowInt64: Int64;
  const HighInt64: Int64;
  const LowNativeInt: NativeInt;
  const HighNativeInt: NativeInt): TClassOfMockUtils;
begin
   WithIntegerRange(LowInteger, HighInteger);
   WithExtendedRange(LowExtended, HighExtended);
   WithDateTimeRange(LowDateTime, HighDateTime);
   WithDateRange(LowDate, HighDate);
   WithTimeRange(LowTime, HighTime);
   WithInt64Range(LowInt64, HighInt64);
   WithNativeIntRange(LowNativeInt, HighNativeInt);

   Result := MockUtils;
end;

class procedure MockUtils.Initialize;
begin
  FNullChance := 0.2;
  FStringPrefix := '';
  FStringLengthLow := 0;
  FStringLengthHigh := 0;
  FLowInteger := -MaxInt;
  FHighInteger := MaxInt;
  FLowExtended := -MaxInt;
  FHighExtended := MaxInt;
  FMaxDecimalDigits := 20;
  FLowDateTime := IncYear(Now, - 100);
  FHighDateTime := IncYear(Now, 100);
  FLowDate := IncYear(Date, -100);
  FHighDate := IncYear(Date, 100);
  FLowTime := Trunc(Date);
  FHighTime := IncMilliSecond(FLowTime, 999);

  Int64Rec(FHighInt64).Hi := MaxInt;
  Int64Rec(FHighInt64).Lo := MaxInt;
  FLowInt64 := -FHighInt64;

  FLowNativeInt := -MaxInt;
  FHighNativeInt := MaxInt;

  FLowSmallint := -32768;
  FHighSmallint := 32767;
end;

initialization
  Randomize;
  MockUtils.Initialize;

end.

