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

unit Fido.Db.DatasetFieldAttributes;

interface

uses
  System.Generics.Defaults,

  Spring.Collections,

  Fido.Types,
  Fido.Db.DatasetFieldAttributes.Intf;

type
  TDatasetFieldAttributes = class(TInterfacedObject, IDatasetFieldAttributes)
  strict private
    FMap: IDictionary<string, TDatasetFieldAttribute>;
    FList: IList<string>;
  public
    constructor Create;

    procedure SetAttribute(const FieldName: string; const Width: Integer; const Title: string; const &ReadOnly: Boolean; const EditMask: string; const Visible: Boolean; const Precision: Integer = 0);
    function TryGetAttribute(const FieldName: string; out Attribute: TDatasetFieldAttribute): Boolean;
    function GetFieldNamesEnumerator: IEnumerator<string>;
    function Count: Integer;
  end;

implementation

{ TDatasetFieldAttributes }

function TDatasetFieldAttributes.Count: Integer;
begin
  Result := FMap.Count;
end;

constructor TDatasetFieldAttributes.Create;
begin
  FMap := TCollections.CreateDictionary<string, TDatasetFieldAttribute>(TIStringComparer.Ordinal);
  FList := TCollections.CreateList<string>(TIStringComparer.Ordinal)
end;

function TDatasetFieldAttributes.GetFieldNamesEnumerator: IEnumerator<string>;
begin
  Result := FList.GetEnumerator;
end;

procedure TDatasetFieldAttributes.SetAttribute(
  const FieldName: string;
  const Width: Integer;
  const Title: string;
  const ReadOnly: Boolean;
  const EditMask: string;
  const Visible: Boolean;
  const Precision: Integer = 0);
begin
  if not FList.Contains(FieldName) then
    FList.Add(FieldName);

  FMap[FieldName] :=
    TDatasetFieldAttribute.Create(
      Width,
      Title,
      ReadOnly,
      EditMask,
      Visible,
      Precision);
end;

function TDatasetFieldAttributes.TryGetAttribute(
  const FieldName: string;
  out Attribute: TDatasetFieldAttribute): Boolean;
begin
  Result := FMap.TryGetValue(FieldName, Attribute);
end;

end.
