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

unit Fido.Types;

interface

uses
  Data.DB,
  Fido.Db.TypeConverter,

  Spring;

type
  TNullableExtended = Nullable<Extended>;
  TNullableSmallint = Nullable<Smallint>;

  TNullableStringArray = TArray<Nullable<string>>;
  TNullableIntegerArray = TArray<Nullable<Integer>>;
  TNullableInt64Array = TArray<Nullable<Int64>>;
  TNullableDoubleArray = TArray<Nullable<Double>>;
  TNullableDateTimeArray = TArray<Nullable<TDateTime>>;
  TNullableGuidArray = TArray<Nullable<TGuid>>;
  TNullableExtendedArray = TArray<Nullable<Extended>>;

  //Useful aliases for TResult<>;
  TRecordsAffected = Integer;

  TDatasetFieldAttribute = record
  private
    FWidth: Integer;
    FTitle: string;
    FReadOnly: Boolean;
    FEditMask: string;
    FVisible: Boolean;
    FPrecision: Integer;
  public
    constructor Create(
      const Width: Integer;
      const Title: string;
      const &ReadOnly: Boolean;
      const EditMask: string;
      const Visible: Boolean;
      const Precision: Integer = 0);

    property Width: Integer read FWidth;
    property Title: string read FTitle;
    property &ReadOnly: Boolean read FReadOnly;
    property EditMask: string read FEditMask;
    property Visible: Boolean read FVisible;
    property Precision: Integer read FPrecision;
  end;

  TResult = record
  strict private
    FSuccess: string;//use records types initialised by compiler.
    FErrorMessage: string;
    function GetSuccess: Boolean;
    procedure SetSuccess(Value: Boolean);
  public
    constructor Create(const Success: Boolean; const ErrorMessage: string);
    class function CreateSuccess: TResult; static;
    constructor CreateFailure(const ErrorMessage: string);

    property Success: Boolean read GetSuccess;
    property ErrorMessage: string read FErrorMessage;
  end;

  TResult<T> = record
  strict private
    FResult: TResult;
    FValue: T;

    function GetErrorMessage: string;
    function GetSuccess: Boolean;
    function GetValue: T;
  public
    constructor Create(const Success: Boolean; const ErrorMessage: string; Value: T); overload;
    constructor Create(const Result: TResult; Value: T); overload;
    constructor CreateSuccess(const Value: T; const Dummy: Integer = 0); overload;  //Extra dummy parameter to remove W1029 warning
    constructor CreateFailure(const ErrorMessage: string); overload;

    class operator Implicit(const value: TResult<T>): TResult; inline;

    property Success: Boolean read GetSuccess;
    property ErrorMessage: string read GetErrorMessage;
    property Value: T read GetValue;
  end;

  // left outside TVirtualStatement class since Delphi dies when trying to create dictionary of it
  // unless using fully-qualified name, which is a nightmare (TVirtualInterface<T>.TParamDescriptor)
  TParamDescriptor = class
  public
    MappedName: string;
    DataType: TDataTypeDescriptor;
    Direction: TParamType;
    Index: integer;
    IsPagingLimit: Boolean;
    IsPagingOffset: Boolean
  end;

  TMethodCategory = (
    mcNone,         // unknown
    mcExecute,      // main Execute method (scalar if function)
    mcDatasetOper,  // allowed dataset operations (only queries), see: TDatasetOperation
    mcRowsAffected, // rows affected on commands
    mcColGetter);   // returns value of a field (only for stQuery)
    // TODO? mcParamGetter,  // returns value of OUT parameter (in case there is more than one)
    // TODO? mcParamSetter,  // sets individual Statement parameter (instead of arguments of a method which set them all)

  TDatasetOperation = (
    dsNone,  // invalida value
    dsNext,  // calls Dataset.Next
    dsFirst, // calls Dataset.First
    dsClose, // calls Dataset.Close
    dsEOF);  // returns Dataset.EOF

  TReturnType = (
    rtNone,    // procedure
    rtInteger, // integer, used by sequences and command to return value and RowsAffected
    rtEnum,    // enumerator (queries)
    rtOther);  // other types: fields in scalar queries, or parameter values in case of procs)

  TMethodDescriptor = class
    OriginalName: string;     // original name of method
    MappedName: string;       // mapped name of column or parameter of a method itself (not its pareameters)
    ReturnType: TReturnType;
    Converter: TDataTypeDescriptor;
    Category: TMethodCategory;
    Operation: TDatasetOperation;
    FieldValue: Variant;
    function GetIsFunction: boolean;
    property IsFunction: boolean read GetIsFunction;
  end;

implementation

{ TDatasetFieldAttribute }

constructor TDatasetFieldAttribute.Create(
  const Width: Integer;
  const Title: string;
  const ReadOnly: Boolean;
  const EditMask: string;
  const Visible: Boolean;
  const Precision: Integer);
begin
  Self.FWidth := Width;
  Self.FTitle := Title;
  Self.FReadOnly := ReadOnly;
  Self.FEditMask := EditMask;
  Self.FVisible := Visible;
  Self.FPrecision := Precision;
end;

{ TResult }

constructor TResult.Create(const Success: Boolean; const ErrorMessage: string);
begin
  SetSuccess(Success);
  FErrorMessage := ErrorMessage;
end;

function TResult.GetSuccess: Boolean;
begin
  Result := FSuccess <> '';
end;

procedure TResult.SetSuccess(Value: Boolean);
begin
  if Value then
    FSuccess := 'Y'
  else
    FSuccess := '';
end;

constructor TResult.CreateFailure(const ErrorMessage: string);
begin
  Self.FSuccess := '';
  Self.FErrorMessage := ErrorMessage;
end;

class function TResult.CreateSuccess: TResult;
begin
  Result.FSuccess := 'Y';
  Result.FErrorMessage := '';
end;

{ TResult<T> }

constructor TResult<T>.Create(const Success: Boolean; const ErrorMessage: string; Value: T);
begin
  Self.FResult := TResult.Create(Success, ErrorMessage);
  Self.FValue := Value;
end;

constructor TResult<T>.Create(const Result: TResult; Value: T);
begin
  Self.FResult := Result;
  Self.FValue := Value;
end;

constructor TResult<T>.CreateSuccess(const Value: T; const Dummy: Integer = 0);
begin
  Create(TResult.Create(True, ''), Value);
end;

constructor TResult<T>.CreateFailure(const ErrorMessage: string);
begin
  Create(TResult.Create(False, ErrorMessage), Default(T));
end;

function TResult<T>.GetErrorMessage: string;
begin
  Result := Self.FResult.ErrorMessage;
end;

function TResult<T>.GetSuccess: Boolean;
begin
  Result := Self.FResult.Success;
end;

function TResult<T>.GetValue: T;
begin
  Result := Self.FValue;
end;

class operator TResult<T>.Implicit(const value: TResult<T>): TResult;
begin
  Result := Value.FResult;
end;

{ TMethodDescriptor }

function TMethodDescriptor.GetIsFunction: boolean;
begin
  Result := ReturnType <> rtNone;
end;

end.
