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

unit Fido.ValueEnumerator;

interface

uses
  System.Rtti,
  Data.DB,

  Spring,
  Spring.Collections;

type
  TValueEnumerator = class(TInterfacedObject, IEnumerator)
  private
    FHasMoved: boolean;
    FDataset: TDataset;
    FOwner: IInterface;
  public
    constructor Create (const Owner: IInterface; const Dataset: TDataset);
    destructor Destroy; override;
    // IEnumerator
    function GetCurrent: TValue;
    function MoveNext: Boolean;
    procedure Reset;
  end;

implementation

{ TValueEnumerator }

constructor TValueEnumerator.Create(const Owner: IInterface; const Dataset: TDataset);
begin
  inherited Create;
  FOwner := Owner;
  FDataset := Dataset;
end;

destructor TValueEnumerator.Destroy;
begin
  FDataset.Close;
  inherited;
end;

function TValueEnumerator.GetCurrent: TValue;
begin
  Result := TValue.From<IInterface>(FOwner);
end;

function TValueEnumerator.MoveNext: Boolean;
begin
  Guard.CheckTrue(FDataset.Active, 'Dataset is not open');
  if not FHasMoved then
  begin
    FHasMoved := true;
    Result := not FDataset.IsEmpty;
  end
  else
  begin
    FDataset.Next;
    Result := not FDataset.Eof;
  end;
end;

procedure TValueEnumerator.Reset;
begin
  Guard.CheckTrue(FDataset.Active, 'Dataset is not open');
  FDataset.First;
  FHasMoved := false;
end;

end.
