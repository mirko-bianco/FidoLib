(*
 * Copyright 2023 Mirko Bianco (email: writetomirko@gmail.com)
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

 unit Fido.Db.StatementExecutor.Zeos;

interface

uses
  System.Rtti,
  Data.DB,

  ZAbstractRODataset,
  ZAbstractDataset,
  ZDataset,
  ZStoredProcedure,

  Fido.Utilities,
  Fido.Types.TGuid.Variant,
  Fido.Db.Connections.Zeos,
  Fido.VirtualStatement.Intf,
  Fido.VirtualStatement.Attributes,
  Fido.StatementExecutor.Intf,
  Fido.StatementExecutor.Abstract;

type
  TZeosStatementExecutor = class (TAbstractStatementExecutor, IStatementExecutor)
  private
    FZeosConnections: TZeosConnections;

    function GetParameters: TParams;
  protected
    function BuildObjectInternal(const StatementType: TStatementType; const SQLData: string): TObject; override;
  public
    constructor Create(ZeosConnections: TZeosConnections);

    function GetParameterValue(const ParamName: string): Variant; override;
    procedure AddParameter(const ParamName: string; const DataType: TFieldType; const ParamType: TParamType = ptInput); override;
    procedure SetParameterValue(const ParamName: string; const Value: Variant); override;
    procedure Prepare; override;
    procedure Execute; override;
  end;

implementation

uses
  System.SysUtils,
  System.Variants;

{ TZeosStatementExecutor }

procedure TZeosStatementExecutor.AddParameter(
  const ParamName: string;
  const DataType: TFieldType;
  const ParamType: TParamType);
var
  P: TParam;
const
  TypeToDirectionMap: array[TParamType] of TParamType = (
    ptUnknown, ptInput, ptOutput, ptInputOutput, ptResult);
begin
  inherited;
  P := GetParameters.FindParam(ParamName);
  if not Assigned(P) then
  begin
    P := GetParameters.AddParameter;
    P.Name := ParamName;
  end;
  P.ParamType := TypeToDirectionMap[ParamType];
  P.DataType := DataType;
end;

function TZeosStatementExecutor.BuildObjectInternal(
  const StatementType: TStatementType;
  const SQLData: string): TObject;
begin
  Result := nil;
  try
    case StatementType of
      stSequence, stQuery, stFunction, stScalarQuery: // = stOpenable
        begin
          Result := TZQuery.Create(nil);
          with TZQuery(Result) do begin
            Connection := FZeosConnections.GetCurrent;
            SQL.Text := SQLData;
          end;
        end;
      stStoredProc:
        begin
          Result := TZStoredProc.Create(nil);
          TZStoredProc(Result).StoredProcName := SQLData;
          TZStoredProc(Result).Connection := FZeosConnections.GetCurrent;
        end;
      stCommand:
        begin
          Result := TZQuery.Create(nil);
          with TZQuery(Result) do begin
            Connection := FZeosConnections.GetCurrent;
            SQL.Text := SQLData;
          end;
        end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

constructor TZeosStatementExecutor.Create(ZeosConnections: TZeosConnections);
begin
  inherited Create;
  FZeosConnections := Utilities.CheckNotNullAndSet(ZeosConnections, 'ZeosConnections');
end;

procedure TZeosStatementExecutor.Execute;
begin
  inherited;
  case StatementType of
    stStoredProc:
      TZStoredProc(Statement).ExecProc;
    stCommand: begin
      TZQuery(Statement).ExecSQL;
      FRowsAffected := TZQuery(Statement).RowsAffected;
    end;
  end;
end;

function TZeosStatementExecutor.GetParameters: TParams;
begin
  Assert(Assigned(Statement));
  case StatementType of
    stStoredProc:
      Result := TZStoredProc(Statement).Params;
    stCommand:
      Result := TZQuery(Statement).Params;
    stQuery, stFunction, stScalarQuery:
      Result := TZQuery(Statement).Params;
    else
      Result := nil;
  end;
end;

function TZeosStatementExecutor.GetParameterValue(const ParamName: string): Variant;
begin
  inherited;
  Result := GetParameters.ParamByName(ParamName).Value;
end;

procedure TZeosStatementExecutor.Prepare;
begin
  inherited;
  case StatementType of
    stStoredProc:
      TZStoredProc(Statement).Prepared := true;
    stCommand, stQuery, stFunction, stSequence, stScalarQuery:
    begin
      TZQuery(Statement).Prepared := true;
    end
    else
      Assert(false, 'Unimplemented');
  end;
end;

procedure TZeosStatementExecutor.SetParameterValue(
  const ParamName: string;
  const Value: Variant);
begin
  case GetParameters.ParamByName(ParamName).DataType of
    ftGuid:
      if VarIsNull(Value) then
        GetParameters.ParamByName(ParamName).Value := Null
      else
        GetParameters.ParamByName(ParamName).AsGUID := StringToGuid(Value);
  else
    GetParameters.ParamByName(ParamName).Value := Value;
  end;
end;

end.
