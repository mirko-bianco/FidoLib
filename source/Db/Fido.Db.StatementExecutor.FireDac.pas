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

 unit Fido.Db.StatementExecutor.FireDac;

interface

uses
  System.Rtti,
  Data.DB,

  FireDac.Stan.Param,
  FireDAC.Comp.Client,

  Spring,

  Fido.Types.TGuid.Variant,
  Fido.Db.Connections.FireDac,
  Fido.VirtualStatement.Intf,
  Fido.VirtualStatement.Attributes,
  Fido.StatementExecutor.Intf,
  Fido.StatementExecutor.Abstract;

type
  TFireDacStatementExecutor = class (TAbstractStatementExecutor, IStatementExecutor)
  private
    FFireDacConnections: TFireDacConnections;

    function GetParameters: TFDParams;
  protected
    function BuildObjectInternal(const StatementType: TStatementType; const SQLData: string): TObject; override;
  public
    constructor Create(FireDacConnections: TFireDacConnections);

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

{ TFireDacStatementExecutor }

procedure TFireDacStatementExecutor.AddParameter(
  const ParamName: string;
  const DataType: TFieldType;
  const ParamType: TParamType);
var
  P: TFDParam;
const
  TypeToDirectionMap: array[TParamType] of TParamType = (
    ptUnknown, ptInput, ptOutput, ptInputOutput, ptResult);
begin
  inherited;
  P := GetParameters.FindParam(ParamName);
  if not Assigned(P) then
  begin
    P := GetParameters.Add;
    P.Name := ParamName;
  end;
  P.ParamType := TypeToDirectionMap[ParamType];
  P.DataType := DataType;
end;

function TFireDacStatementExecutor.BuildObjectInternal(
  const StatementType: TStatementType;
  const SQLData: string): TObject;
begin
  Result := nil;
  try
    case StatementType of
      stSequence, stQuery, stFunction: // = stOpenable
        begin
          Result := TFDQuery.Create(nil);
          with TFDQuery(Result) do begin
            Connection := FFireDacConnections.GetCurrent;
            SQL.Text := SQLData;
          end;
        end;
      stStoredProc:
        begin
          Result := TFDStoredProc.Create(nil);
          TFDStoredProc(Result).StoredProcName := SQLData;
          TFDStoredProc(Result).Connection := FFireDacConnections.GetCurrent;
        end;
      stCommand:
        begin
          Result := TFDQuery.Create(nil);
          with TFDQuery(Result) do begin
            Connection := FFireDacConnections.GetCurrent;
            SQL.Text := SQLData;
          end;
        end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

constructor TFireDacStatementExecutor.Create(FireDacConnections: TFireDacConnections);
begin
  inherited Create;
  Guard.CheckNotNull(FireDacConnections, 'FireDacConnections');
  FFireDacConnections := FireDacConnections;
end;

procedure TFireDacStatementExecutor.Execute;
begin
  inherited;
  case StatementType of
    stStoredProc:
      TFDStoredProc(Statement).ExecProc;
    stCommand: begin
      TFDQuery(Statement).ExecSQL;
      FRowsAffected := TFDQuery(Statement).RowsAffected;
    end;
  end;
end;

function TFireDacStatementExecutor.GetParameters: TFDParams;
begin
  Assert(Assigned(Statement));
  case StatementType of
    stStoredProc:
      Result := TFDStoredProc(Statement).Params;
    stCommand:
      Result := TFDQuery(Statement).Params;
    stQuery, stFunction:
      Result := TFDQuery(Statement).Params;
    else
      Result := nil;
  end;
end;

function TFireDacStatementExecutor.GetParameterValue(const ParamName: string): Variant;
begin
  inherited;
  Result := GetParameters.ParamByName(ParamName).Value;
end;

procedure TFireDacStatementExecutor.Prepare;
begin
  inherited;
  case StatementType of
    stStoredProc:
      TFDStoredProc(Statement).Prepared := true;
    stCommand, stQuery, stFunction, stSequence:
    begin
      TFDQuery(Statement).FetchOptions.Unidirectional := True;
      if (FPagingOffset > -1) and (FPagingLimit > -1) then
      begin
        TFDQuery(Statement).FetchOptions.RecsSkip := FPagingOffset;
        TFDQuery(Statement).FetchOptions.RecsMax := FPagingLimit;
      end;
      TFDQuery(Statement).Prepared := true;
    end
    else
      Assert(false, 'Unimplemented');
  end;
end;

procedure TFireDacStatementExecutor.SetParameterValue(
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
