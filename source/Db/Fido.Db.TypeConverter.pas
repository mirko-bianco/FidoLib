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

unit Fido.Db.TypeConverter;

interface

uses
  System.TypInfo,
  System.Rtti,
  System.Types,
  System.Variants,
  Data.DB,

  Spring,
  Spring.Collections,

  Fido.Exceptions,
  Fido.Types.TGuid.Variant;

type
  TBaseType = (btVariant, btInteger, btString, btDouble, btDate, btDateTime, btInt64, btBoolean, btCurrency, btExtended, btGuid, btSmallint,
    btEnum);

  EFidoUnsupportedTypeError = class (EFidoException);

  TDataTypeDescriptor = class
  private
    TypeName: string;
    IsNullable: boolean;
    FBaseType: TBaseType;
    FFieldType: TFieldType;
  public
    function GetAsVariant(const V: TValue): Variant;
    function GetFromVariant(const V: Variant): TValue;

    property BaseType: TBaseType read FBaseType;
    property FieldType: TFieldType read FFieldType;
  end;

  DataTypeConverter = class
  strict private const
    SpringNullableTemplate = 'SPRING.NULLABLE<%s>';
    BaseTypeNames : array[TBaseType] of string = (
      'SYSTEM.VARIANT', 'SYSTEM.INTEGER', 'SYSTEM.STRING', 'SYSTEM.DOUBLE',
      'SYSTEM.TDATE', 'SYSTEM.TDATETIME', 'SYSTEM.INT64', 'SYSTEM.BOOLEAN',
      'SYSTEM.CURRENCY', 'SYSTEM.EXTENDED', 'SYSTEM.TGUID', 'SYSTEM.SMALLINT',
      'SYSTEM.ENUM');
  strict private class var
      FTypeCache: IDictionary<string, TDataTypeDescriptor>;
  strict private
    class procedure AddTypeDescriptor(const BaseType: TBaseType; const IsNullable: boolean; const TypeKind: TTypeKind; const FieldType: TFieldType; const TypeName: string = '');
  public
    class constructor Create;
    class function GotDescriptor(const RttiType: TRttiType; out Descriptor: TDataTypeDescriptor): boolean; overload;
    class function GotDescriptor(const QualifiedTypeName: string; out Descriptor: TDataTypeDescriptor): boolean; overload;
    class function GetDescriptor(const RttiType: TRttiType): TDataTypeDescriptor;
  end;

implementation

uses
  System.SysUtils;

{ DataTypesConverter }

class procedure DataTypeConverter.AddTypeDescriptor(
  const BaseType: TBaseType;
  const IsNullable: boolean;
  const TypeKind: TTypeKind;
  const FieldType: TFieldType;
  const TypeName: string = '');
var
  D: TDataTypeDescriptor;
begin
  D := TDataTypeDescriptor.Create;
  try
    if not TypeName.IsEmpty then
      D.TypeName := TypeName.ToUpper
    else if IsNullable then
      D.TypeName := Format(SpringNullableTemplate, [BaseTypeNames[BaseType]]).ToUpper
    else
      D.TypeName := BaseTypeNames[BaseType].ToUpper;

    // D.TypeKind := TypeKind;
    D.FBaseType := BaseType;
    D.FFieldType := FieldType;
    D.IsNullable := IsNullable;

    FTypeCache.Add(D.TypeName, D);
  except
    D.Free;
    raise;
  end;
end;

class constructor DataTypeConverter.Create;
begin
  FTypeCache := TCollections.CreateDictionary<string, TDataTypeDescriptor>([doOwnsValues]);

  // RTTI integers  (ignore ShortInt, Smallint etc.)
  AddTypeDescriptor(btInteger, false, tkInteger, ftInteger);
  AddTypeDescriptor(btInt64, false, tkInt64, ftLargeInt);

  // RTTI strings (ignore: tkChar, tkString, tkWChar, tkLString, tkWString, tkUString)
  AddTypeDescriptor(btString, false, tkString, ftString);

  // RTTI floats (ignore TTime, we don't really use it)
  AddTypeDescriptor(btDouble, false, tkFloat, ftFloat);
  AddTypeDescriptor(btDate, false, tkFloat, ftDate);
  AddTypeDescriptor(btDateTime, false, tkFloat, ftDateTime);
  AddTypeDescriptor(btCurrency, false, tkFloat, ftCurrency);
  AddTypeDescriptor(btExtended, false, tkFloat, ftExtended);


  // RTTI Boolean (ignore rest of enumerables)
  AddTypeDescriptor(btBoolean, false, tkEnumeration, ftBoolean);

  // plain variant
  AddTypeDescriptor(btVariant, false, tkVariant, ftVariant);

  // Guid
  AddTypeDescriptor(btGuid, false, tkRecord, ftGuid);

  AddTypeDescriptor(btSmallint, false, tkInteger, ftSmallint);

  // Spring Nullables for all supported primitives
  // TypeKind = tkRecord
  AddTypeDescriptor(btInteger, true, tkRecord, ftInteger);
  AddTypeDescriptor(btInt64, true, tkRecord, ftLargeInt);
  AddTypeDescriptor(btString, true, tkRecord, ftString);
  AddTypeDescriptor(btDouble, true, tkRecord, ftFloat);
  AddTypeDescriptor(btDate, true, tkRecord, ftDate);
  AddTypeDescriptor(btDateTime, true, tkRecord, ftDateTime);
  AddTypeDescriptor(btCurrency, true, tkRecord, ftCurrency);
  AddTypeDescriptor(btExtended, true, tkRecord, ftExtended);
  AddTypeDescriptor(btBoolean, true, tkRecord, ftBoolean);
  AddTypeDescriptor(btGuid, true, tkRecord, ftGuid);
  AddTypeDescriptor(btSmallint, true, tkRecord, ftSmallint);


  AddTypeDescriptor(btEnum, false, tkEnumeration, ftInteger);
end;

class function DataTypeConverter.GetDescriptor(const RttiType: TRttiType): TDataTypeDescriptor;
begin
  if not GotDescriptor(RttiType, Result) then
    raise EFidoUnsupportedTypeError.CreateFmt('Type %s is not supported by Fido Framework', [RttiType.QualifiedName]);
end;

class function DataTypeConverter.GotDescriptor(
  const QualifiedTypeName: string;
  out Descriptor: TDataTypeDescriptor): boolean;
begin
  Result := FTypeCache.TryGetValue(QualifiedTypeName.ToUpper, Descriptor);
end;

class function DataTypeConverter.GotDescriptor(
  const RttiType: TRttiType;
  out Descriptor: TDataTypeDescriptor): boolean;
var
  _RttiTypeQualifiedName: string;
begin
  _RttiTypeQualifiedName := RttiType.QualifiedName;
  if (RttiType.TypeKind = tkEnumeration) and (not RttiType.QualifiedName.ToUpper.Equals('SYSTEM.BOOLEAN')) then
     _RttiTypeQualifiedName := 'SYSTEM.ENUM';

  Result := GotDescriptor(_RttiTypeQualifiedName, Descriptor);
end;

{ TDataTypeDescriptor }

function TDataTypeDescriptor.GetAsVariant(const V: TValue): Variant;
begin
  if not IsNullable then
  begin
    case BaseType of
      btGuid:
        if V.AsType<TGuid> = GUID_NULL then
          Exit(Null)
        else
          Exit(VarGuidCreate(V.AsType<TGuid>));
      else
        Exit(V.AsVariant)
    end;
  end
  else
    case BaseType of
      btInteger: Exit(V.AsType<Nullable<integer>>.ToVariant);
      btString: Exit(V.AsType<Nullable<string>>.ToVariant);
      btDouble: Exit(V.AsType<Nullable<double>>.ToVariant);
      btDate: Exit(V.AsType<Nullable<TDate>>.ToVariant);
      btDateTime: Exit(V.AsType<Nullable<TDateTime>>.ToVariant);
      btCurrency: Exit(V.AsType<Nullable<Currency>>.ToVariant);
      btExtended: Exit(V.AsType<Nullable<Extended>>.ToVariant);
      btInt64: Exit(V.AsType<Nullable<Int64>>.ToVariant);
      btBoolean: Exit(V.AsType<Nullable<Boolean>>.ToVariant);
      btGuid: Exit(V.AsType<Nullable<TGuid>>.ToVariant);
      btSmallint: Exit(V.AsType<Nullable<Smallint>>.ToVariant);
      else
        Assert(false, 'Unsupported nullable type');
    end;

  Result := null;
end;

function TDataTypeDescriptor.GetFromVariant(const V: Variant): TValue;
begin
  if not IsNullable then
  begin
    case BaseType of
      btGuid:
        if V = Null then
          Exit(TValue.From<TGuid>(TGuid.Empty))
        else
          Exit(TValue.From<TGuid>(StringToGuid(V)));
      else
        Exit(TValue.FromVariant(V));
    end;
  end
  else
    case BaseType of
      btInteger: Exit(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(V)));
      btString: Exit(TValue.From<Nullable<string>>(Nullable<string>.Create(V)));
      btDouble: Exit(TValue.From<Nullable<double>>(Nullable<double>.Create(V)));
      btDate: Exit(TValue.From<Nullable<TDate>>(Nullable<TDate>.Create(V)));
      btDateTime: Exit(TValue.From<Nullable<TDateTime>>(Nullable<TDateTime>.Create(V)));
      btCurrency: Exit(TValue.From<Nullable<Currency>>(Nullable<Currency>.Create(V)));
      btExtended: Exit(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(V)));
      btInt64: Exit(TValue.From<Nullable<Int64>>(Nullable<Int64>.Create(V)));
      btBoolean: Exit(TValue.From<Nullable<Boolean>>(Nullable<Boolean>.Create(V)));
      btSmallint: Exit(TValue.From<Nullable<Smallint>>(Nullable<Smallint>.Create(V)));
      btGuid:
        if V = null then
          Exit(TValue.From<Nullable<TGuid>>(Nullable<TGuid>.Create(V)))
        else
          Exit(TValue.From<Nullable<TGuid>>(Nullable<TGuid>.Create(StringToGuid(V))));
      else
        Assert(false, 'Unsupported nullable type');
    end;

  Result := TValue.Empty;
end;

end.
