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

unit Fido.Resource.StringReader;

interface

uses
  Spring,
  Fido.Resource.StreamReader.Intf,
  Fido.Resource.StringReader.Intf;

type
  TStringResourceReader = class(TInterfacedObject, IStringResourceReader)
  private
    FStreamResourceReader: IStreamResourceReader;
  public
    constructor Create(const StreamResourceReader: IStreamResourceReader);
    function GetStringResource(const ResName: string): string;
  end;

implementation

uses
  System.Classes;

{ TStringResourceReader }

constructor TStringResourceReader.Create(const StreamResourceReader: IStreamResourceReader);
begin
  Guard.CheckNotNull(StreamResourceReader, 'StreamResourceReader');
  FStreamResourceReader := StreamResourceReader;
end;

function TStringResourceReader.GetStringResource(const ResName: string): string;
var
  ResourceStream: Shared<TStream>;
  StringStream: Shared<TStringStream>;
begin
  ResourceStream := FStreamResourceReader.GetResourceStream(ResName);
  StringStream := TStringStream.Create;

  StringStream.Value.LoadFromStream(ResourceStream);
  Result := StringStream.Value.DataString;
end;

end.
