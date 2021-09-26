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

unit Fido.Db.DatasetFieldAttributes.Intf;

interface

uses
  System.Generics.Defaults,

  Spring.Collections,

  Fido.Types;

type
  IDatasetFieldAttributes = interface
    ['{0A138581-4D0D-4BBB-8DB8-FC9C5FB5CCB7}']

    procedure SetAttribute(
      const FieldName: string;
      const Width: Integer;
      const Title: string;
      const &ReadOnly: Boolean;
      const EditMask: string;
      const Visible: Boolean;
      const Precision: Integer = 0);

    function TryGetAttribute(const FieldName: string; out Attribute: TDatasetFieldAttribute): Boolean;

    function GetFieldNamesEnumerator: IEnumerator<string>;

    function Count: Integer;
  end;

implementation

end.
