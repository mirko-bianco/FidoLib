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

unit Fido.StatementExecutor.Abstract;

interface

uses
  System.SysUtils,
  System.Rtti,
  Data.DB,

  Spring,

  Fido.Exceptions,
  Fido.StatementExecutor.Intf,
  Fido.VirtualStatement.Attributes,
  Fido.VirtualStatement.Intf;

type
  EFidoStatementExecutorError = class(EFidoException);

  TAbstractStatementExecutor = class (TInterfacedObject, IStatementExecutor)
  strict private type
    TStatementTypes = set of TStatementType;
  strict private var
    FStatementType: TStatementType;
    FStatement: TObject;
  protected var
    FRowsAffected: Integer;
    FPagingLimit: Integer;
    FPagingOffset: Integer;
  protected
    procedure RaiseError(const Msg: string; const Args: array of const);
    procedure AssertTypeIn(const AllowedTypes: TStatementTypes);
    procedure AssertIsQuery(const AndIsOpen: boolean = false);
    function BuildObjectInternal(const StatementType: TStatementType; const SQLData: string): TObject; virtual; abstract;

    property Statement: TObject read FStatement;
    property StatementType: TStatementType read FStatementType;
  public
    constructor Create;
    destructor Destroy; override;

    // IStatementExecutor
    function GetIsBuilt: boolean;
    function GetFieldByName(const FieldName: string): TField;
    function GetParameterValue(const ParamName: string): Variant; virtual;
    function GetRowsAffected: integer;
    procedure AddParameter(const ParamName: string; const DataType: TFieldType; const ParamType: TParamType = ptInput); virtual;
    procedure SetParameterValue(const ParamName: string; const Value: Variant); virtual; abstract;
    procedure BuildObject(const StatementType: TStatementType; const SQLData: string);
    procedure Prepare; virtual; abstract;
    procedure SetPaging(const PagingLimit: Integer; const PagingOffset: Integer);
    function Open: TDataset;
    procedure Execute; virtual;

    property IsBuilt: boolean read GetIsBuilt;
  end;

implementation

{ TAbstractStatementExecutor }

procedure TAbstractStatementExecutor.AddParameter(
  const ParamName: string;
  const DataType: TFieldType;
  const ParamType: TParamType);
begin
  AssertTypeIn(stParametrised);
  // .. implement rest in descendant
end;

procedure TAbstractStatementExecutor.AssertIsQuery(const AndIsOpen: boolean = false);
begin
  AssertTypeIn(stOpenable);
  Assert((Statement is TDataset) and (not AndIsOpen or TDataset(Statement).Active));
end;

procedure TAbstractStatementExecutor.AssertTypeIn(const AllowedTypes: TStatementTypes);
begin
  Assert(IsBuilt);
  if not (StatementType in AllowedTypes) then
    RaiseError('Call not allowed for statement type %d', [Ord(StatementType)]);
end;

procedure TAbstractStatementExecutor.BuildObject(
  const StatementType: TStatementType;
  const SQLData: string);
begin
  FreeAndNil(FStatement);
  FRowsAffected := 0;
  FStatementType := stNone;

  FStatement := BuildObjectInternal(StatementType, SQLData);
  FStatementType := StatementType;
  Assert(IsBuilt);

  if StatementType in stOpenable then
    AssertIsQuery;
end;

constructor TAbstractStatementExecutor.Create;
begin
  inherited Create;
  FPagingLimit := -1;
  FPagingOffset := -1;
end;

destructor TAbstractStatementExecutor.Destroy;
begin
  FStatement.Free;
  inherited;
end;

procedure TAbstractStatementExecutor.Execute;
begin
  AssertTypeIn(stExecutable);
  // .. implement rest in descendant
end;

function TAbstractStatementExecutor.GetFieldByName(const FieldName: string): TField;
begin
  AssertIsQuery(true);
  Result := TDataset(FStatement).FieldByName(FieldName);
end;

function TAbstractStatementExecutor.GetIsBuilt: boolean;
begin
  Result := Assigned(FStatement);
  Assert(not Result or (StatementType in stValid));
end;

function TAbstractStatementExecutor.GetParameterValue(const ParamName: string): Variant;
begin
  AssertTypeIn(stParametrised);
  // .. implement rest in descendant
end;

function TAbstractStatementExecutor.GetRowsAffected: integer;
begin
  AssertTypeIn([stCommand]);
  Result := FRowsAffected;
end;

function TAbstractStatementExecutor.Open: TDataset;
begin
  AssertIsQuery;
  Result := TDataset(FStatement);
  with Result do
  begin
    if Active then
      Close;
    Open;
  end;
end;

procedure TAbstractStatementExecutor.RaiseError(
  const Msg: string;
  const Args: array of const);
begin
  raise EFidoStatementExecutorError.CreateFmt(Msg, Args);
end;

procedure TAbstractStatementExecutor.SetPaging(
  const PagingLimit: Integer;
  const PagingOffset: Integer);
begin
  FPagingLimit := PagingLimit;
  FPagingOffset := PagingOffset;
  Prepare;
end;

end.
