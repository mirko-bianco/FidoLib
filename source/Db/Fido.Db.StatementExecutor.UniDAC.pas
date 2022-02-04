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

 unit Fido.Db.StatementExecutor.UniDAC;

interface

uses
  System.Rtti,
  Data.DB,

  Uni,

  Spring,

  Fido.Types.TGuid.Variant,
  Fido.Db.Connections.UniDAC,
  Fido.VirtualStatement.Intf,
  Fido.VirtualStatement.Attributes,
  Fido.StatementExecutor.Intf,
  Fido.StatementExecutor.Abstract;

type
  TUniDACStatementExecutor = class (TAbstractStatementExecutor, IStatementExecutor)
  private
    FUniDACConnections: TUniDACConnections;

    function GetParameters: TUniParams;
  protected
    function BuildObjectInternal(const StatementType: TStatementType; const SQLData: string): TObject; override;
  public
    constructor Create(UniDACConnections: TUniDACConnections);

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

{ TUniDACStatementExecutor }

procedure TUniDACStatementExecutor.AddParameter(
  const ParamName: string;
  const DataType: TFieldType;
  const ParamType: TParamType);
var
  P: TUniParam;
const
  TypeToDirectionMap: array[TParamType] of TParamType = (
    ptUnknown, ptInput, ptOutput, ptInputOutput, ptResult);
begin
  inherited;
  P := GetParameters.FindParam(ParamName);
  if not Assigned(P) then
  begin
    P := GetParameters.AddParameter as TUniParam;
    P.Name := ParamName;
  end;
  P.ParamType := TypeToDirectionMap[ParamType];
  P.DataType := DataType;
end;

function TUniDACStatementExecutor.BuildObjectInternal(
  const StatementType: TStatementType;
  const SQLData: string): TObject;
begin
  Result := nil;
  try
    case StatementType of
      stSequence, stQuery, stFunction: // = stOpenable
        begin
          Result := TUniQuery.Create(nil);
          with TUniQuery(Result) do begin
            Connection := FUniDACConnections.GetCurrent;
            SQL.Text := SQLData;
            {$if defined(DEBUG)}
            Debug := True;
            {$ifend}
          end;
        end;
      stStoredProc:
        begin
          Result := TUniStoredProc.Create(nil);
          TUniStoredProc(Result).StoredProcName := SQLData;
          TUniStoredProc(Result).Connection := FUniDACConnections.GetCurrent;
        end;
      stCommand:
        begin
          Result := TUniQuery.Create(nil);
          with TUniQuery(Result) do begin
            Connection := FUniDACConnections.GetCurrent;
            SQL.Text := SQLData;
            {$if defined(DEBUG)}
            Debug := True;
            {$ifend}
          end;
        end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

constructor TUniDACStatementExecutor.Create(UniDACConnections: TUniDACConnections);
begin
  inherited Create;
  Guard.CheckNotNull(UniDACConnections, 'UniDACConnections');
  FUniDACConnections := UniDACConnections;
end;

procedure TUniDACStatementExecutor.Execute;
begin
  inherited;
  case StatementType of
    stStoredProc:
      TUniStoredProc(Statement).ExecProc;
    stCommand: begin
      TUniQuery(Statement).ExecSQL;
      FRowsAffected := TUniQuery(Statement).RowsAffected;
    end;
  end;
end;

function TUniDACStatementExecutor.GetParameters: TUniParams;
begin
  Assert(Assigned(Statement));
  case StatementType of
    stStoredProc:
      Result := TUniStoredProc(Statement).Params;
    stCommand:
      Result := TUniQuery(Statement).Params;
    stQuery, stFunction:
      Result := TUniQuery(Statement).Params;
    else
      Result := nil;
  end;
end;

function TUniDACStatementExecutor.GetParameterValue(const ParamName: string): Variant;
begin
  inherited;
  Result := GetParameters.ParamByName(ParamName).Value;
end;

procedure TUniDACStatementExecutor.Prepare;
begin
  inherited;
  case StatementType of
    stStoredProc:
      TUniStoredProc(Statement).Prepared := true;
    stCommand, stQuery, stFunction, stSequence:
    begin
      TUniQuery(Statement).Unidirectional := True;
      if (FPagingOffset > -1) and (FPagingLimit > -1) then
      begin
        TUniQuery(Statement).FetchRows := FPagingOffset;
      end;
      TUniQuery(Statement).Connection := FUniDACConnections.GetCurrent;
      TUniQuery(Statement).Prepared := true;
    end
    else
      Assert(false, 'Unimplemented');
  end;
end;

procedure TUniDACStatementExecutor.SetParameterValue(
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
