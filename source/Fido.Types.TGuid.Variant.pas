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

 unit Fido.Types.TGuid.Variant;

interface

uses
  System.SysUtils,
  System.Variants;

type
  TGuidVarData = packed record
     VType: TVarType;
     Reserved1, Reserved2, Reserved3: Word;
     VGuid: PGuid;
     Reserved4 : Cardinal;
     {$IFDEF CPUX64}
     Reserved5 : Cardinal;
     {$ENDIF}
  end;

  TGuidVariantType = class(TCustomVariantType)
  public
    function IsClear(const V: TVarData): Boolean; override;
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    function CompareOp(const Left, Right: TVarData; const Operator: TVarOp): Boolean; override;
  end;

function VarGuidCreate(const AGuid: TGuid): Variant;

implementation

var
  GuidVariantType: TGuidVariantType;

function VarGuidCreate(const AGuid: TGuid): Variant;
begin
    VarClear(Result);
    TGuidVarData(Result).VType := GuidVariantType.VarType;
    New(TGuidVarData(Result).VGuid);
    TGuidVarData(Result).VGuid^ := AGuid;
end;

function TGuidVariantType.IsClear(const V: TVarData): Boolean;
const
  EmptyGuid: TGuid = '{00000000-0000-0000-0000-000000000000}';
begin
  Result := (TGuidVarData(V).VGuid = nil) or CompareMem(TGuidVarData(V).VGuid, @EmptyGuid, SizeOf(TGuid));
end;

procedure TGuidVariantType.Cast(var Dest: TVarData; const Source: TVarData);
var
  LSource, LTemp: TVarData;
begin
  VarDataInit(LSource);
  try
    VarDataCopyNoInd(LSource, Source);
    if VarDataIsStr(LSource) then
    begin
      New(TGuidVarData(Dest).VGuid);
      TGuidVarData(Dest).VGuid^ := StringToGuid(VarDataToStr(LSource));
    end else
    begin
      VarDataInit(LTemp);
      try
        VarDataCastTo(LTemp, LSource, varString);
        New(TGuidVarData(Dest).VGuid);
        TGuidVarData(Dest).VGuid^ := StringToGuid(VarDataToStr(LTemp));
      finally
        VarDataClear(LTemp);
      end;
    end;
    Dest.VType := VarType;
  finally
    VarDataClear(LSource);
  end;
end;

procedure TGuidVariantType.CastTo(var Dest: TVarData; const Source: TVarData;
const AVarType: TVarType);
var
  LTemp: TVarData;
begin
  if Source.VType = VarType then
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, GuidToString(TGuidVarData(Source).VGuid^));
      varString:
        VarDataFromStr(Dest, GuidToString(TGuidVarData(Source).VGuid^));
    else
      VarDataInit(LTemp);
      try
        VarDataFromStr(LTemp, GuidToString(TGuidVarData(Source).VGuid^));
        VarDataCastTo(Dest, LTemp, AVarType);
      finally
        VarDataClear(LTemp);
      end;
    end
  else
    RaiseCastError;
end;

procedure TGuidVariantType.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  Dispose(TGuidVarData(V).VGuid);
  TGuidVarData(V).VGuid := nil;
end;

procedure TGuidVariantType.Copy(var Dest: TVarData; const Source: TVarData;
const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
  begin
    TGuidVarData(Dest).VType := TGuidVarData(Source).VType;
    New(TGuidVarData(Dest).VGuid);
    TGuidVarData(Dest).VGuid^ := TGuidVarData(Source).VGuid^;
  end;
end;

function TGuidVariantType.CompareOp(const Left, Right: TVarData; const Operator:
TVarOp): Boolean;
begin
  Result := False;
  if (Left.VType = VarType) and (Right.VType = VarType) then
    case Operator of
      opCmpEQ:
        Result := CompareMem(TGuidVarData(Left).VGuid, TGuidVarData(Right).VGuid,
SizeOf(TGuid));
      opCmpNE:
        Result := not CompareMem(TGuidVarData(Left).VGuid, TGuidVarData(Right).VGuid,
SizeOf(TGuid));
    else
      RaiseInvalidOp;
    end
  else
    RaiseInvalidOp;
end;

function VarGuid: TVarType;
begin
  Result := GuidVariantType.VarType;
end;

function VarIsGuid(const AValue: Variant): Boolean;
begin
  Result := (TVarData(AValue).VType and varTypeMask) = VarGuid;
end;

function VarAsGuid(const AValue: Variant): Variant;
begin
  if not VarIsGuid(AValue) then
    VarCast(Result, AValue, VarGuid)
  else
    Result := AValue;
end;

initialization
  GuidVariantType := TGuidVariantType.Create;
finalization
  FreeAndNil(GuidVariantType);

end.
