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

unit Fido.Caching.Intf;

interface

uses
  Fido.Types;

type
  IOneParamCache<P, R> = Interface(IInvokable)
    ['{60819F67-2B66-46BA-8F78-22C3597E4FEB}']

    // It executes the function if it has not been cached yet and then caches it, otherwise retrieves the value from the cache
    function It(const AFunction: TOneParamFunction<P, R>; const Param: P): R;
    // ForceIt always executes the function and then caches it. Useful when you want to re-cache stale data
    function ForceIt(const AFunction: TOneParamFunction<P, R>; const Param: P): R;
  end;

  ITwoParamsCache<P1, P2, R> = interface(IInvokable)
    ['{BCD8DB8D-25A8-4991-B1FB-B6213B03A556}']

    function It(const AFunction: TTwoParamsFunction<P1, P2, R>; const Param1: P1; const Param2: P2): R;
    function ForceIt(const AFunction: TTwoParamsFunction<P1, P2, R>; const Param1: P1; const Param2: P2): R;
  end;

  IThreeParamsCache<P1, P2, P3, R> = interface(IInvokable)
    ['{DDC1A3B3-8E6E-4F1B-BFC8-474D7F186237}']

    function It(const AFunction: TThreeParamsFunction<P1, P2, P3, R>; const Param1: P1; const Param2: P2; const Param3: P3): R;
    function ForceIt(const AFunction: TThreeParamsFunction<P1, P2, P3, R>; const Param1: P1; const Param2: P2; const Param3: P3): R;
  end;

  IFourParamsCache<P1, P2, P3, P4, R> = interface(IInvokable)
    ['{6C1E8BDD-9805-4095-8179-BC0230F9EC19}']

    function It(const AFunction: TFourParamsFunction<P1, P2, P3, P4, R>; const Param1: P1; const Param2: P2; const Param3: P3; const Param4: P4): R;
    function ForceIt(const AFunction: TFourParamsFunction<P1, P2, P3, P4, R>; const Param1: P1; const Param2: P2; const Param3: P3; const Param4: P4): R;
  end;

implementation

end.
