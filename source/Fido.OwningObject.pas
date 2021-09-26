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

unit Fido.OwningObject;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo;

type
  // An owning object will atomatically free any instance that is part of its
  // properties. It will also free the instances in an array of objects.
  TOwningObject = class
  private
    procedure FreeClassProperties; inline;
  public
    destructor Destroy; override;
  end;

implementation

destructor TOwningObject.Destroy;
begin
  FreeClassProperties;

  inherited;
end;

procedure TOwningObject.FreeClassProperties;
var
  i: Integer;
  RttiContext: TRttiContext;
  RttiProperty: TRttiProperty;
  Value: TValue;
begin
  for RttiProperty in RttiContext.GetType(Self.ClassType).GetProperties do
  begin
    if RttiProperty.PropertyType.TypeKind = tkClass then
    begin

      Value := RttiProperty.GetValue(Self);
      try
        TObject(PPointer(Value.GetReferenceToRawData)^).Free;
      except
      end;
    end
    else if RttiProperty.PropertyType.TypeKind = tkDynArray then
    begin

      if TRttiDynamicArrayType(RttiProperty.PropertyType).ElementType.TypeKind = tkClass then
      begin
        Value := RttiProperty.GetValue(Self);

        for i := 0 to Value.GetArrayLength - 1 do
          try
            TObject(PPointer(Value.GetArrayElement(i).GetReferenceToRawData)^).Free;
          except
          end;
      end;

    end;
  end;
end;

end.
