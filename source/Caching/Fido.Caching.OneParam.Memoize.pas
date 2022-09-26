(*
 * Copyright 2022 Mirko Bianco (email: writetomirko@gmail.com)
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

unit Fido.Caching.OneParam.Memoize;

interface

uses
  System.SysUtils,

  Spring.Collections,

  Fido.Types,
  Fido.Caching.Intf;

type
  TMemoizeOneParam<P, R> = class(TInterfacedObject, IOneParamCache<P, R>)
  private var
    FLock: IReadWriteSync;
    FMap: IDictionary<P, R>;
  public
    constructor Create;
    function It(const AFunction: TOneParamFunction<P, R>; const Param: P): R;
    function ForceIt(const AFunction: TOneParamFunction<P, R>; const Param: P): R;
  end;

implementation

{ TMemoizeOneParam<P, R> }

constructor TMemoizeOneParam<P, R>.Create;
begin
  inherited;
  FLock := TMREWSync.Create;
  FMap := TCollections.CreateDictionary<P, R>;
end;

function TMemoizeOneParam<P, R>.ForceIt(const AFunction: TOneParamFunction<P, R>; const Param: P): R;
begin
  FLock.BeginWrite;
  try
    Result := AFunction(Param);
    FMap[Param] := Result;
  finally
    FLock.EndWrite;
  end;
end;

function TMemoizeOneParam<P, R>.It(const AFunction: TOneParamFunction<P, R>; const Param: P): R;
begin
  FLock.BeginWrite;
  try
    if FMap.TryGetValue(Param, Result) then
      Exit;
    Result := AFunction(Param);
    FMap[Param] := Result;
  finally
    FLock.EndWrite;
  end;
end;

end.

