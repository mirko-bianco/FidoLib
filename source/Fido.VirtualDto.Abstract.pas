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

unit Fido.VirtualDto.Abstract;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.Classes,
  Data.DB,

  Spring.Collections,

  Fido.Db.TypeConverter,
  Fido.VirtualStatement.Attributes,
  Fido.VirtualInterface,
  Fido.Types;

type
  TAbstractVirtualDto<T: IInterface> = class abstract (TVirtualInterface<T>)
  strict private const
    GetterPrefix = 'GET';
  strict private
    function GetIsGetterName(const Name: string): boolean;

  protected
    function GetMappedName(const Name: string): string;
    procedure DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue); virtual; abstract;
    procedure ProcessDtoAttributes; virtual; abstract;
    procedure ProcessMethod(const Method: TRttiMethod); virtual; abstract;
  public
    constructor Create; reintroduce;

    procedure AfterConstruction; override;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  System.Generics.Collections;

{ TAbstractVirtualRecord<T> }

procedure TAbstractVirtualDto<T>.AfterConstruction;
begin
  inherited;
  ProcessDtoAttributes;
end;

constructor TAbstractVirtualDto<T>.Create;
begin
  inherited Create(DoInvoke);
end;

function TAbstractVirtualDto<T>.GetIsGetterName(const Name: string): boolean;
begin
  Result := Name.StartsWith(GetterPrefix, true);
end;

function TAbstractVirtualDto<T>.GetMappedName(const Name: string): string;
var
  Prefix: string;
begin
  // TODO use actual Maps
  Result := Name.ToUpper;

  // remove getter prefix; TODO setters?
  if GetIsGetterName(Result) then
    Delete(Result, 1, Length(GetterPrefix));
end;

end.
