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

unit Fido.Caching;

interface

uses
  Spring,

  Fido.Caching.Intf,
  Fido.Caching.OneParam.FIFO,
  Fido.Caching.OneParam.Usage,
  Fido.Caching.OneParam.Memoize,
  Fido.Caching.TwoParams,
  Fido.Caching.ThreeParams,
  Fido.Caching.FourParams;

type
  Caching = class
  type
    OneParam = class
      class function Usage<P, R>(const Size: Int64): IOneParamCache<P, R>; static;
      class function FIFO<P, R>(const Size: Int64): IOneParamCache<P, R>; static;
      class function Memoize<P, R>: IOneParamCache<P, R>; static;
    end;
    TwoParams = class
      class function Usage<P1, P2, R>(const Size: Int64): ITwoParamsCache<P1, P2, R>; static;
      class function FIFO<P1, P2, R>(const Size: Int64): ITwoParamsCache<P1, P2, R>; static;
      class function Memoize<P1, P2, R>: ITwoParamsCache<P1, P2, R>; static;
    end;
    ThreeParams = class
      class function Usage<P1, P2, P3, R>(const Size: Int64): IThreeParamsCache<P1, P2, P3, R>; static;
      class function FIFO<P1, P2, P3, R>(const Size: Int64): IThreeParamsCache<P1, P2, P3, R>; static;
      class function Memoize<P1, P2, P3, R>: IThreeParamsCache<P1, P2, P3, R>; static;
    end;
    FourParams = class
      class function Usage<P1, P2, P3, P4, R>(const Size: Int64): IFourParamsCache<P1, P2, P3, P4, R>; static;
      class function FIFO<P1, P2, P3, P4, R>(const Size: Int64): IFourParamsCache<P1, P2, P3, P4, R>; static;
      class function Memoize<P1, P2, P3, P4, R>: IFourParamsCache<P1, P2, P3, P4, R>; static;
    end;
  end;

implementation

{ Caching.OneParam }

class function Caching.OneParam.FIFO<P, R>(const Size: Int64): IOneParamCache<P, R>;
begin
  Result := TFIFOOneParamCache<P, R>.Create(Size);
end;

class function Caching.OneParam.Memoize<P, R>: IOneParamCache<P, R>;
begin
  Result := TMemoizeOneParam<P, R>.Create;
end;

class function Caching.OneParam.Usage<P, R>(const Size: Int64): IOneParamCache<P, R>;
begin
  Result := TUsageOneParamCache<P, R>.Create(Size);
end;

{ Caching.TwoParams }

class function Caching.TwoParams.FIFO<P1, P2, R>(const Size: Int64): ITwoParamsCache<P1, P2, R>;
begin
  Result := TTwoParamsCache<P1, P2, R>.Create(
    function: IOneParamCache<P1, Func<P2, R>>
    begin
      Result := TFIFOOneParamCache<P1, Func<P2, R>>.Create(Size);
    end,
    function: IOneParamCache<P2, R>
    begin
      Result := TFIFOOneParamCache<P2, R>.Create(Size);
    end);
end;

class function Caching.TwoParams.Memoize<P1, P2, R>: ITwoParamsCache<P1, P2, R>;
begin
  Result := TTwoParamsCache<P1, P2, R>.Create(
    function: IOneParamCache<P1, Func<P2, R>>
    begin
      Result := TMemoizeOneParam<P1, Func<P2, R>>.Create;
    end,
    function: IOneParamCache<P2, R>
    begin
      Result := TMemoizeOneParam<P2, R>.Create;
    end);
end;

class function Caching.TwoParams.Usage<P1, P2, R>(const Size: Int64): ITwoParamsCache<P1, P2, R>;
begin
  Result := TTwoParamsCache<P1, P2, R>.Create(
    function: IOneParamCache<P1, Func<P2, R>>
    begin
      Result := TUsageOneParamCache<P1, Func<P2, R>>.Create(Size);
    end,
    function: IOneParamCache<P2, R>
    begin
      Result := TUsageOneParamCache<P2, R>.Create(Size);
    end);
end;

{ Caching.ThreeParams }

class function Caching.ThreeParams.FIFO<P1, P2, P3, R>(const Size: Int64): IThreeParamsCache<P1, P2, P3, R>;
begin
  Result := TThreeParamsCache<P1, P2, P3, R>.Create(
    function: IOneParamCache<P1, Func<P2, Func<P3, R>>>
    begin
      Result := TFIFOOneParamCache<P1, Func<P2, Func<P3, R>>>.Create(Size);
    end,
    function: IOneParamCache<P2, Func<P3, R>>
    begin
      Result := TFIFOOneParamCache<P2, Func<P3, R>>.Create(Size);
    end,
    function: IOneParamCache<P3, R>
    begin
      Result := TFIFOOneParamCache<P3, R>.Create(Size);
    end);
end;

class function Caching.ThreeParams.Memoize<P1, P2, P3, R>: IThreeParamsCache<P1, P2, P3, R>;
begin
  Result := TThreeParamsCache<P1, P2, P3, R>.Create(
    function: IOneParamCache<P1, Func<P2, Func<P3, R>>>
    begin
      Result := TMemoizeOneParam<P1, Func<P2, Func<P3, R>>>.Create;
    end,
    function: IOneParamCache<P2, Func<P3, R>>
    begin
      Result := TMemoizeOneParam<P2, Func<P3, R>>.Create;
    end,
    function: IOneParamCache<P3, R>
    begin
      Result := TMemoizeOneParam<P3, R>.Create;
    end);
end;

class function Caching.ThreeParams.Usage<P1, P2, P3, R>(const Size: Int64): IThreeParamsCache<P1, P2, P3, R>;
begin
  Result := TThreeParamsCache<P1, P2, P3, R>.Create(
    function: IOneParamCache<P1, Func<P2, Func<P3, R>>>
    begin
      Result := TUsageOneParamCache<P1, Func<P2, Func<P3, R>>>.Create(Size);
    end,
    function: IOneParamCache<P2, Func<P3, R>>
    begin
      Result := TUsageOneParamCache<P2, Func<P3, R>>.Create(Size);
    end,
    function: IOneParamCache<P3, R>
    begin
      Result := TUsageOneParamCache<P3, R>.Create(Size);
    end);
end;

{ Caching.FourParams }

class function Caching.FourParams.FIFO<P1, P2, P3, P4, R>(const Size: Int64): IFourParamsCache<P1, P2, P3, P4, R>;
begin
  Result := TFourParamsCache<P1, P2, P3, P4, R>.Create(
    function: IOneParamCache<P1, Func<P2, Func<P3, Func<P4, R>>>>
    begin
      Result := TFIFOOneParamCache<P1, Func<P2, Func<P3, Func<P4, R>>>>.Create(Size);
    end,
    function: IOneParamCache<P2, Func<P3, Func<P4, R>>>
    begin
      Result := TFIFOOneParamCache<P2, Func<P3, Func<P4, R>>>.Create(Size);
    end,
    function: IOneParamCache<P3, Func<P4, R>>
    begin
      Result := TFIFOOneParamCache<P3, Func<P4, R>>.Create(Size);
    end,
    function: IOneParamCache<P4, R>
    begin
      Result := TFIFOOneParamCache<P4, R>.Create(Size);
    end);
end;

class function Caching.FourParams.Memoize<P1, P2, P3, P4, R>: IFourParamsCache<P1, P2, P3, P4, R>;
begin
  Result := TFourParamsCache<P1, P2, P3, P4, R>.Create(
    function: IOneParamCache<P1, Func<P2, Func<P3, Func<P4, R>>>>
    begin
      Result := TMemoizeOneParam<P1, Func<P2, Func<P3, Func<P4, R>>>>.Create;
    end,
    function: IOneParamCache<P2, Func<P3, Func<P4, R>>>
    begin
      Result := TMemoizeOneParam<P2, Func<P3, Func<P4, R>>>.Create;
    end,
    function: IOneParamCache<P3, Func<P4, R>>
    begin
      Result := TMemoizeOneParam<P3, Func<P4, R>>.Create;
    end,
    function: IOneParamCache<P4, R>
    begin
      Result := TMemoizeOneParam<P4, R>.Create;
    end);
end;

class function Caching.FourParams.Usage<P1, P2, P3, P4, R>(const Size: Int64): IFourParamsCache<P1, P2, P3, P4, R>;
begin
  Result := TFourParamsCache<P1, P2, P3, P4, R>.Create(
    function: IOneParamCache<P1, Func<P2, Func<P3, Func<P4, R>>>>
    begin
      Result := TUsageOneParamCache<P1, Func<P2, Func<P3, Func<P4, R>>>>.Create(Size);
    end,
    function: IOneParamCache<P2, Func<P3, Func<P4, R>>>
    begin
      Result := TUsageOneParamCache<P2, Func<P3, Func<P4, R>>>.Create(Size);
    end,
    function: IOneParamCache<P3, Func<P4, R>>
    begin
      Result := TUsageOneParamCache<P3, Func<P4, R>>.Create(Size);
    end,
    function: IOneParamCache<P4, R>
    begin
      Result := TUsageOneParamCache<P4, R>.Create(Size);
    end);
end;

end.
