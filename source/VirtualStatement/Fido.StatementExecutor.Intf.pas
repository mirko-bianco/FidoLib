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

unit Fido.StatementExecutor.Intf;

interface

uses
  System.Rtti,
  Data.DB,

  Fido.VirtualStatement.Attributes,
  Fido.VirtualStatement.Intf;

type
  IStatementExecutor = interface
    ['{63D2D54A-7BA4-42D5-8D5C-38B1D8D59155}']
    function GetIsBuilt: boolean;
    function GetFieldByName(const FieldName: string): TField;
    function GetParameterValue(const ParamName: string): Variant;
    function GetRowsAffected: integer;

    procedure AddParameter(const ParamName: string; const DataType: TFieldType; const ParamType: TParamType = ptInput);
    procedure SetParameterValue(const ParamName: string; const Value: Variant);

    procedure SetPaging(const PagingLimit: Integer; const PagingOffset: Integer);

    procedure BuildObject(const StatementType: TStatementType; const SQLData: string);
    procedure Prepare;

    function Open: TDataset;
    procedure Execute;

    property IsBuilt: boolean read GetIsBuilt;
  end;

implementation

end.
