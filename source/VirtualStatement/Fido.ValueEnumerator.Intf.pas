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

unit Fido.ValueEnumerator.Intf;

interface

uses
  Spring.Collections;

type
{ IEnumerator can be used to go through all query rows.
  It is redeclared here since there are two: in System and Spring.Collections
  As nobody declares System in uses (esp. not after this unit) this declaration
  should be sufficient to enforce Spring.Collections.IEnumerator as the right type

  Typical query definition would be then:

     IMyQuery = interface(IVirtualStatement)
       function GetFieldA: integer;
       function GetFieldB: string;

       function Open(const param1: integer): IEnumerator;

       property FieldB: string read GetFieldB;
     end;

  usage:

    MyQuery: IMyQuery;

    with MyQuery.Open(1234) do
      while MoveNext do
        ComboBox1.Items.AddObject(FieldB);

  NOTE: once Enumerator goes out of scope the dataset is closed automatically }

  IEnumerator = Spring.Collections.IEnumerator;

const
  ValueEnumeratorTypeName = 'Spring.Collections.IEnumerator';

implementation

end.
