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

unit Fido.Virtual.Attributes;

interface

type
  PagingLimitAttribute = class(TCustomAttribute);

  PagingOffsetAttribute = class(TCustomAttribute);

{ SqlInject parameters allow to change SQL scripts in a way that is not allowed by normal parameters

  Example:

  [Statement(stQuery, 'Q_AN_EXAMPLE_QUERY')]
  function Open(const [SqlInject('ORDERBY')] OrderBy: string);

  Will replace the tag %ORDERBY% of the sql resource called Q_AN_EXAMPLE_QUERY with the content of the OrderBy parameter:
  'select * from somequery order by %ORDERBY%' }

  SqlInjectAttribute = class(TCustomAttribute)
  private
    FTag: string;
  public
    constructor Create(const Tag: string);

    property Tag: string read FTag;
  end;

implementation

{ SqlInjectAttribute }

constructor SqlInjectAttribute.Create(const Tag: string);
begin
  inherited Create;

  FTag := Tag;
end;

end.
