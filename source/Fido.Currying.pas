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

unit Fido.Currying;

interface

uses
  Fido.Types;

type
  TCurrying3Result<P2, P3, R> = reference to function(const Parameter2: P2): TOneParamFunction<P3, R>;
  TCurrying3Func<P1, P2, P3, R> = reference to function(const Parameter1: P1): TCurrying3Result<P2, P3, R>;

  TCurrying4Result<P2, P3, P4, R> = reference to function(const Parameter2: P2): TCurrying3Result<P3, P4, R>;
  TCurrying4Func<P1, P2, P3, P4, R> = reference to function(const Parameter1: P1): TCurrying4Result<P2, P3, P4, R>;

  Curry = record
    class function Cook<P1, P2, R>(const SourceFunction: TTwoParamsFunction<P1, P2, R>): TOneParamFunction<P1, TOneParamFunction<P2, R>>; overload; static;
    class function Cook<P1, P2, P3, R>(const SourceFunction: TThreeParamsFunction<P1, P2, P3, R>): TCurrying3Func<P1, P2, P3, R>; overload; static;
    class function Cook<P1, P2, P3, P4, R>(const SourceFunction: TFourParamsFunction<P1, P2, P3, P4, R>): TCurrying4Func<P1, P2, P3, P4, R>; overload; static;
  end;

implementation

{ Curry }

class function Curry.Cook<P1, P2, R>(const SourceFunction: TTwoParamsFunction<P1, P2, R>): TOneParamFunction<P1, TOneParamFunction<P2, R>>;
begin
  Result := function(const Parameter1: P1): TOneParamFunction<P2, R>
    begin
      result := function(const Parameter2: P2): R
        begin
          result := SourceFunction(Parameter1, Parameter2);
        end;
    end;
end;

class function Curry.Cook<P1, P2, P3, R>(const SourceFunction: TThreeParamsFunction<P1, P2, P3, R>): TCurrying3Func<P1, P2, P3, R>;
begin
  Result := function(const Parameter1: P1): TCurrying3Result<P2, P3, R>
    begin
      result := function(const Parameter2: P2): TOneParamFunction<P3, R>
        begin
          Result := function(const Parameter3: P3): R
            begin
              result := SourceFunction(Parameter1, Parameter2, Parameter3);
            end;
        end;
    end;
end;

class function Curry.Cook<P1, P2, P3, P4, R>(const SourceFunction: TFourParamsFunction<P1, P2, P3, P4, R>): TCurrying4Func<P1, P2, P3, P4, R>;
begin
  Result := function(const Parameter1: P1): TCurrying4Result<P2, P3, P4, R>
    begin
      result := function(const Parameter2: P2): TCurrying3Result<P3, P4, R>
        begin
          Result := function(const Parameter3: P3): TOneParamFunction<P4, R>
            begin
              Result := function(const Parameter4: P4): R
                begin
                  result := SourceFunction(Parameter1, Parameter2, Parameter3, Parameter4);
                end;
            end;
        end;
    end;
end;

end.
