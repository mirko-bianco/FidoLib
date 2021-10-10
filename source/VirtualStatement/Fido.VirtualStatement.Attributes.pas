(*
 * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
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

unit Fido.VirtualStatement.Attributes;

interface

type
  TStatementType = (
    stNone,        // unknown/unset
    stSequence,    // table sequence, see: IVirtualSequence
    stFunction,    // function (will use select .. from dual without a need for resource)
    stQuery,       // SELECT statement (both scalar and sets), see: IVirtualQuery
    stCommand,     // INSERT, UPDATE, DELETE or MERGE command, see: IVirtualCommand
    stStoredProc) ;// stored procedure (incl. returning values)

const
  // types that are valid to use with virtual statment parser and executor
  stValid = [stSequence .. stStoredProc];
  // types that require fully qualified name in statement data string
  stQualified = [stSequence, stFunction, stStoredProc];
  // types that will be treated as openable queries
  stOpenable = [stSequence, stQuery, stFunction];
  // types that MUST have a scalar Execute method (i.e. function returning only one value)
  stScalar = [stSequence, stFunction];
  // types that will be executed (as opposed to opened)
  stExecutable = [stCommand, stStoredProc];
  // types that can have parameters (all valid except for stSequence)
  stParametrised = [stFunction, stQuery, stCommand, stStoredProc];
  // types that require SQL resource name in statment data
  stResourced = [stQuery, stCommand];

type
{ Statement attribute is REQUIRED (and sufficient). It defines what to do with a statement

  1. based on SQL resources

    [Statement(stQuery, 'Q_REPL_LIST_STORES')]
    [Statement(stCommand, 'Q_REPL_ORDER_UPDPURCHORDERSUBM')]

  2. based on fully-qualified DB object names

    [Statement(stFunction, 'PKG_SESSION.GETCONTEXTPARAMETER')]
    [Statement(stStoredProc, 'DEV.PRC_INTERCOMPANYORDER')]
    [Statement(stSequence, 'DEV.SEQ_INTERNALORDERID')] }

  StatementAttribute = class(TCustomAttribute)
  private
    FType: TStatementType;
    FData: string;
  public
    constructor Create(const &Type: TStatementType; const Data: string);

    property Data: string read FData;
    property &Type: TStatementType read FType;
  end;

{ Execute is an attribute of one method to be used to open/execute statement.
  If it has arguments, they will be used as statement parameters, e.g.

    [Execute]
    procedure Load(const StoreId: integer);

  If defined as function it is treated as scalar statement, returning column
  or out parameter corresponding to method's name & closing dataset, e.g.

    [Execute]
    function GetSubsidiaryId(const StoreId: integer): integer;

   - sets "STOREID" parameter to argument value and opens the query
   - returns "SUBSIDIARYID" field value and closes immediately
   - can be mixed with Param/Column/Map attributes (see below)

  NOTE if you don't set this attribute, method named "Execute" will be treated as such }

  ExecuteAttribute = class(TCustomAttribute);

{ TStringAttribute is an ancestor of all attributes with string parameter.
  DO NOT use as a standalone attribute, e.g. [TString('TEXT')] }

  TStringAttribute = class(TCustomAttribute)
  private
    FLine: string;
  public
    constructor Create(const Line: string);

    property Line: string read FLine write FLine;
  end;

{ Column can be used to force different column name than the gettere evalues to, e.g.

   [Column('STOCKLOCATIONID')]
   function GetStoreId: integer;

   function will return value of STOCKLOCATIONID rather than STOREID}

  ColumnAttribute = class(TStringAttribute);

{ Description contains explanation what statement does (OPTIONAL) }

  DescriptionAttribute = class(TStringAttribute);

  PagingLimitAttribute = class(TCustomAttribute);

  PagingOffsetAttribute = class(TCustomAttribute);

implementation

Uses
  System.SysUtils;

{ TStringAttribute }

constructor TStringAttribute.Create(const Line: string);
begin
  FLine := Line;
end;

{ StatementAttribute }

constructor StatementAttribute.Create(
  const &Type: TStatementType;
  const Data: string);
begin
  FType := &Type;
  FData := Data.ToUpper;
end;

end.
