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
  Spring,

  Fido.Types;

type
  Curry = record
    class function Cook<P1, R>(const SourceFunction: Func<P1, R>): Func<P1, Func<R>>; overload; static;
    class function Cook<P1, P2, R>(const SourceFunction: Func<P1, P2, R>): Func<P1, Func<P2, R>>; overload; static;
    class function Cook<P1, P2, P3, R>(const SourceFunction: Func<P1, P2, P3, R>): Func<P1, Func<P2, Func<P3, R>>>; overload; static;
    class function Cook<P1, P2, P3, P4, R>(const SourceFunction: Func<P1, P2, P3, P4, R>): Func<P1, Func<P2, Func<P3, Func<P4, R>>>>; overload; static;

    class function Cook<P1>(const SourceProcedure: Action<P1>): Func<P1, Action>; overload; static;
    class function Cook<P1, P2>(const SourceProcedure: Action<P1, P2>): Func<P1, Action<P2>>; overload; static;
    class function Cook<P1, P2, P3>(const SourceProcedure: Action<P1, P2, P3>): Func<P1, Func<P2, Action<P3>>>; overload; static;
    class function Cook<P1, P2, P3, P4>(const SourceProcedure: Action<P1, P2, P3, P4>): Func<P1, Func<P2, Func<P3, Action<P4>>>>; overload; static;
  end;

implementation

{ Curry }

class function Curry.Cook<P1, P2, R>(const SourceFunction: Func<P1, P2, R>): Func<P1, Func<P2, R>>;
begin
  Result := function(const Parameter1: P1): Func<P2, R>
    begin
      result := function(const Parameter2: P2): R
        begin
          result := SourceFunction(Parameter1, Parameter2);
        end;
    end;
end;

class function Curry.Cook<P1, P2>(const SourceProcedure: Action<P1, P2>): Func<P1, Action<P2>>;
begin
  Result := function(const Parameter1: P1): Action<P2>
    begin
      result := procedure(const Parameter2: P2)
        begin
          SourceProcedure(Parameter1, Parameter2);
        end;
    end;
end;

class function Curry.Cook<P1, R>(const SourceFunction: Func<P1, R>): Func<P1, Func<R>>;
begin
  Result := function(const Parameter1: P1): Func<R>
    begin
      result := function: R
        begin
          Result := SourceFunction(Parameter1);
        end;
    end;
end;

class function Curry.Cook<P1>(const SourceProcedure: Action<P1>): Func<P1, Action>;
begin
  Result := function(const Parameter1: P1): Action
    begin
      result := procedure
        begin
          SourceProcedure(Parameter1);
        end;
    end;
end;

class function Curry.Cook<P1, P2, P3, P4>(const SourceProcedure: Action<P1, P2, P3, P4>): Func<P1, Func<P2, Func<P3, Action<P4>>>>;
begin
  Result := function(const Parameter1: P1): Func<P2, Func<P3, Action<P4>>>
    begin
      result := function(const Parameter2: P2): Func<P3, Action<P4>>
        begin
          Result := function(const Parameter3: P3): Action<P4>
            begin
              Result := procedure(const Parameter4: P4)
                begin
                  SourceProcedure(Parameter1, Parameter2, Parameter3, Parameter4);
                end;
            end;
        end;
    end;
end;

class function Curry.Cook<P1, P2, P3, R>(const SourceFunction: Func<P1, P2, P3, R>): Func<P1, Func<P2, Func<P3, R>>>;
begin
  Result := function(const Parameter1: P1): Func<P2, Func<P3, R>>
    begin
      result := function(const Parameter2: P2): Func<P3, R>
        begin
          Result := function(const Parameter3: P3): R
            begin
              result := SourceFunction(Parameter1, Parameter2, Parameter3);
            end;
        end;
    end;
end;

class function Curry.Cook<P1, P2, P3>(const SourceProcedure: Action<P1, P2, P3>): Func<P1, Func<P2, Action<P3>>>;
begin
  Result := function(const Parameter1: P1): Func<P2, Action<P3>>
    begin
      result := function(const Parameter2: P2): Action<P3>
        begin
          Result := procedure(const Parameter3: P3)
            begin
              SourceProcedure(Parameter1, Parameter2, Parameter3);
            end;
        end;
    end;
end;

class function Curry.Cook<P1, P2, P3, P4, R>(const SourceFunction: Func<P1, P2, P3, P4, R>): Func<P1, Func<P2, Func<P3, Func<P4, R>>>>;
begin
  Result := function(const Parameter1: P1): Func<P2, Func<P3, Func<P4, R>>>
    begin
      result := function(const Parameter2: P2): Func<P3, Func<P4, R>>
        begin
          Result := function(const Parameter3: P3): Func<P4, R>
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
