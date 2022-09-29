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

unit Fido.Caching.FourParams;

interface

uses
  System.SysUtils,

  Spring.Collections,

  Fido.Types,
  Fido.Currying,
  Fido.Caching.Intf;

type
  TFourParamsCache<P1, P2, P3, P4, R> = class(TInterfacedObject, IFourParamsCache<P1, P2, P3, P4, R>)
  private
    FCurryCache: IOneParamCache<P1, TOneParamFunction<P2, TOneParamFunction<P3, TOneParamFunction<P4, R>>>>;
    FCurryCache2: IOneParamCache<P2, TOneParamFunction<P3, TOneParamFunction<P4, R>>>;
    FCurryCache3: IOneParamCache<P3, TOneParamFunction<P4, R>>;
    FCache: IOneParamCache<P4, R>;
  public
    constructor Create(
      const CurryCacheFactory: TFunc<IOneParamCache<P1, TOneParamFunction<P2, TOneParamFunction<P3, TOneParamFunction<P4, R>>>>>;
      const CurryCache2Factory: TFunc<IOneParamCache<P2, TOneParamFunction<P3, TOneParamFunction<P4, R>>>>;
      const CurryCache3Factory: TFunc<IOneParamCache<P3, TOneParamFunction<P4, R>>>;
      const CacheFactory: TFunc<IOneParamCache<P4, R>>);

    function It(const AFunction: TFourParamsFunction<P1, P2, P3, P4, R>; const Param1: P1; const Param2: P2; const Param3: P3; const Param4: P4): R;
    function ForceIt(const AFunction: TFourParamsFunction<P1, P2, P3, P4, R>; const Param1: P1; const Param2: P2; const Param3: P3; const Param4: P4): R;
  end;

implementation

{ TFourParamsCache<P1, P2, P3, P4, R> }

constructor TFourParamsCache<P1, P2, P3, P4, R>.Create(
  const CurryCacheFactory: TFunc<IOneParamCache<P1, TOneParamFunction<P2, TOneParamFunction<P3, TOneParamFunction<P4, R>>>>>;
  const CurryCache2Factory: TFunc<IOneParamCache<P2, TOneParamFunction<P3, TOneParamFunction<P4, R>>>>;
  const CurryCache3Factory: TFunc<IOneParamCache<P3, TOneParamFunction<P4, R>>>;
  const CacheFactory: TFunc<IOneParamCache<P4, R>>);
begin
  inherited Create;
  FCurryCache := CurryCacheFactory();
  FCurryCache2 := CurryCache2Factory();
  FCurryCache3 := CurryCache3Factory();
  FCache := CacheFactory();
end;

function TFourParamsCache<P1, P2, P3, P4, R>.ForceIt(const AFunction: TFourParamsFunction<P1, P2, P3, P4, R>; const Param1: P1; const Param2: P2; const Param3: P3; const Param4: P4): R;
begin
  Result := FCache.ForceIt(FCurryCache3.ForceIt(FCurryCache2.ForceIt(FCurryCache.ForceIt(Curry.Cook<P1, P2, P3, P4, R>(AFunction), Param1), Param2), Param3), Param4);
end;

function TFourParamsCache<P1, P2, P3, P4, R>.It(const AFunction: TFourParamsFunction<P1, P2, P3, P4, R>; const Param1: P1; const Param2: P2; const Param3: P3; const Param4: P4): R;
begin
  Result := FCache.It(FCurryCache3.It(FCurryCache2.It(FCurryCache.It(Curry.Cook<P1, P2, P3, P4, R>(AFunction), Param1), Param2), Param3), Param4);
end;

end.
