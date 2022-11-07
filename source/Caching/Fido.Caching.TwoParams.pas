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

unit Fido.Caching.TwoParams;

interface

uses
  System.SysUtils,

  Spring,
  Spring.Collections,

  Fido.Currying,
  Fido.Caching.Intf;

type
  TTwoParamsCache<P1, P2, R> = class(TInterfacedObject, ITwoParamsCache<P1, P2, R>)
  private
    FCurryCache: IOneParamCache<P1, Func<P2, R>>;
    FCache: IOneParamCache<P2, R>;
  public
    constructor Create(
      const CurryCacheFactory: TFunc<IOneParamCache<P1, Func<P2, R>>>;
      const CacheFactory: TFunc<IOneParamCache<P2, R>>);

    function It(const AFunction: Func<P1, P2, R>; const Param1: P1; const Param2: P2): R;
    function ForceIt(const AFunction: Func<P1, P2, R>; const Param1: P1; const Param2: P2): R;
  end;

implementation

{ TTwoParamsCache<P1, P2, R> }

constructor TTwoParamsCache<P1, P2, R>.Create(
  const CurryCacheFactory: TFunc<IOneParamCache<P1, Func<P2, R>>>;
  const CacheFactory: TFunc<IOneParamCache<P2, R>>);
begin
  inherited Create;
  FCurryCache := CurryCacheFactory();
  FCache := CacheFactory();
end;

function TTwoParamsCache<P1, P2, R>.ForceIt(
  const AFunction: Func<P1, P2, R>;
  const Param1: P1;
  const Param2: P2): R;
begin
  Result := FCache.ForceIt(FCurryCache.ForceIt(Curry.Cook<P1, P2, R>(AFunction), Param1), Param2);
end;

function TTwoParamsCache<P1, P2, R>.It(
  const AFunction: Func<P1, P2, R>;
  const Param1: P1;
  const Param2: P2): R;
begin
  Result := FCache.It(FCurryCache.It(Curry.Cook<P1, P2, R>(AFunction), Param1), Param2);
end;

end.
