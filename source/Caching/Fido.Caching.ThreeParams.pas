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

unit Fido.Caching.ThreeParams;

interface

uses
  System.SysUtils,

  Spring.Collections,

  Fido.Types,
  Fido.Currying,
  Fido.Caching.Intf;

type
  TThreeParamsCache<P1, P2, P3, R> = class(TInterfacedObject, IThreeParamsCache<P1, P2, P3, R>)
  private
    FCurryCache: IOneParamCache<P1, TOneParamFunction<P2, TOneParamFunction<P3, R>>>;
    FCurryCache2: IOneParamCache<P2, TOneParamFunction<P3, R>>;
    FCache: IOneParamCache<P3, R>;
  public
    constructor Create(
      const CurryCacheFactory: TFunc<IOneParamCache<P1, TOneParamFunction<P2, TOneParamFunction<P3, R>>>>;
      const CurryCache2Factory: TFunc<IOneParamCache<P2, TOneParamFunction<P3, R>>>;
      const CacheFactory: TFunc<IOneParamCache<P3, R>>);

    function It(const AFunction: TThreeParamsFunction<P1, P2, P3, R>; const Param1: P1; const Param2: P2; const Param3: P3): R;
    function ForceIt(const AFunction: TThreeParamsFunction<P1, P2, P3, R>; const Param1: P1; const Param2: P2; const Param3: P3): R;
  end;

implementation

{ TThreeParamsCache<P1, P2, P3, R> }

constructor TThreeParamsCache<P1, P2, P3, R>.Create(
  const CurryCacheFactory: TFunc<IOneParamCache<P1, TOneParamFunction<P2, TOneParamFunction<P3, R>>>>;
  const CurryCache2Factory: TFunc<IOneParamCache<P2, TOneParamFunction<P3, R>>>;
  const CacheFactory: TFunc<IOneParamCache<P3, R>>);
begin
  inherited Create;
  FCurryCache := CurryCacheFactory();
  FCurryCache2 := CurryCache2Factory();
  FCache := CacheFactory();
end;

function TThreeParamsCache<P1, P2, P3, R>.ForceIt(const AFunction: TThreeParamsFunction<P1, P2, P3, R>; const Param1: P1; const Param2: P2; const Param3: P3): R;
begin
  Result := FCache.ForceIt(FCurryCache2.ForceIt(FCurryCache.ForceIt(Curry.Cook<P1, P2, P3, R>(AFunction), Param1), Param2), Param3);
end;

function TThreeParamsCache<P1, P2, P3, R>.It(const AFunction: TThreeParamsFunction<P1, P2, P3, R>; const Param1: P1; const Param2: P2; const Param3: P3): R;
begin
  Result := FCache.It(FCurryCache2.It(FCurryCache.It(Curry.Cook<P1, P2, P3, R>(AFunction), Param1), Param2), Param3);
end;

end.
