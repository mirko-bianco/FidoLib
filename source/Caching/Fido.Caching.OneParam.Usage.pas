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

unit Fido.Caching.OneParam.Usage;

interface

uses
  System.SyncObjs,

  Spring,
  Spring.Collections,

  Fido.Caching.Intf;

type
  TUsageOneParamCache<P, R> = class(TInterfacedObject, IOneParamCache<P, R>)
  private var
    FSize: Int64;
    FLock: TLightweightMREW;
    FMap: IDictionary<P, R>;
    FAgeList: IList<P>;
  public
    constructor Create(const Size: Int64);
    function It(const AFunction: Func<P, R>; const Param: P): R;
    function ForceIt(const AFunction: Func<P, R>; const Param: P): R;
  end;

implementation

{ TUsageOneParamCache<P, R> }

constructor TUsageOneParamCache<P, R>.Create(const Size: Int64);
begin
  inherited Create;

  FMap := TCollections.CreateDictionary<P, R>;
  FAgeList := TCollections.CreateList<P>;
  FSize := Size;
  if FSize <= 0 then
    FSize := 1;
end;

function TUsageOneParamCache<P, R>.ForceIt(const AFunction: Func<P, R>; const Param: P): R;
var
  Exists: Boolean;
  Index: Integer;
begin
  FLock.BeginWrite;
  try
    Result := AFunction(Param);
    Exists := FMap.ContainsKey(Param);
    FMap[Param] := Result;
    if Exists then
      Exit;

    Index := FAgeList.IndexOf(Param);
    if Index > -1 then
      FAgeList.Delete(Index);
    FAgeList.Add(Param);

    if FAgeList.Count > FSize then
    begin
      FMap.Remove(FAgeList[0]);
      FAgeList.Delete(0);
    end;
  finally
    FLock.EndWrite;
  end;
end;

function TUsageOneParamCache<P, R>.It(const AFunction: Func<P, R>; const Param: P): R;
var
  Exists: Boolean;
begin
  FLock.BeginWrite;
  try
    Exists := FMap.TryGetValue(Param, Result);
    if Exists then
      FAgeList.Delete(FAgeList.IndexOf(Param))
    else
    begin
      Result := AFunction(Param);
      FMap[Param] := Result;
    end;
    FAgeList.Add(Param);
    if FAgeList.Count > FSize then
    begin
      FMap.Remove(FAgeList[0]);
      FAgeList.Delete(0);
    end;
  finally
    FLock.EndWrite;
  end;
end;

end.

