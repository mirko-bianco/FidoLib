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

unit Fido.DesignPatterns.Adapter.DataSetAsReadonlyList;

interface

uses
  DB,

  Spring.Collections,
  Spring.Collections.Lists,

  Fido.VirtualDto.Database;

type
  TDataSetAsReadonlyList<T: IInterface> = class(TAnonymousReadOnlyList<T>, IReadOnlyList<T>)
  private
    FCurrentPos: Integer;
  public
    constructor Create(const Dataset: TDataset); reintroduce;
  end;

implementation

{ TDataSetAsReadonlyList<T> }

constructor TDataSetAsReadonlyList<T>.Create(const Dataset: TDataset);
begin
  inherited Create(
    function: Integer
    begin
      Result := DataSet.RecordCount;
    end,
    function(const Index: Integer): T
    begin
      Dataset.MoveBy(Index - FCurrentPos);
      FcurrentPos := Index;
      Result := DatabaseVirtualDto<T>.New(Dataset);
    end);
  FCurrentPos := 0;
  Dataset.First;
end;

end.
