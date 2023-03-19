(*
 * Copyright 2023 Mirko Bianco (email: writetomirko@gmail.com)
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

unit Fido.Gui.Fmx.Grid.ViewModel;

interface

uses
  System.SysUtils,
  System.Generics.Defaults,
  System.Rtti,
  System.Math,
  FMX.Grid,

  Spring,
  Spring.Collections,

  Fido.Exceptions,
  Fido.Utilities,
  Fido.DesignPatterns.Observer.Intf,
  Fido.DesignPatterns.Observer.Notification.Intf,
  Fido.DesignPatterns.Observable.Intf,
  Fido.DesignPatterns.Observable.Delegated,
  Fido.Collections.DeepObservableList.Intf,
  Fido.Collections,

  Fido.Gui.Fmx.Grid.ViewModel.Intf;

type
  TAbstractReadonlyGridViewModel<T: IObservable> = class abstract (TDelegatedObservable, IReadonlyGridViewModel<T>, IObserver)
  private type
    TGetColumnIndexByCol = reference to function(const Row: Integer): Integer;
    TGetSelectedRow = reference to function: Integer;
    TGetSelectedColumn = reference to function: Integer;
  private var
    FGetColumnIndexByCol: TGetColumnIndexByCol;
    FGetSelectedRow: TGetSelectedRow;
    FGetSelectedColumn: TGetSelectedColumn;
    FData: IList<T>;
    FViewData: IList<T>;
    FUpdateGrid: Action<IList<T>>;
  protected
    function GetColumnIndexByCol(const Row: Integer): Integer;
    function Data: IList<T>; virtual;
    function ViewData: IList<T>; virtual;
    procedure GridGetValue(Sender: TObject; const Col, Row: Integer; var Value: TValue); virtual; abstract;
    function DecorateData(const Data: IList<T>): IList<T>; virtual;
  public
    constructor Create(const Grid: TGrid; const Data: IList<T>);

    procedure Notify(const Sender: IInterface; const Notification: INotification);

    function OnGetFieldValue: TOnGetValue;

    procedure SetData(const Data: IList<T>);
    function SelectedRow: Integer;
    function SelectedColumn: Integer;
    function SelectedItem: T;
  end;

  TAbstractWriteableGridView<T: IObservable> = class abstract (TAbstractReadonlyGridViewModel<T>, IWriteableGridViewModel<T>)
  protected
    procedure GridSetValue(Sender: TObject; const Col, Row: Integer; const Value: TValue); virtual; abstract;
  public
    function OnSetFieldValue: TOnSetValue;
  end;

  TFilteredGridViewModelHelper<T: IObservable; FilterPayload> = class(TInterfacedObject, IFilteredGridViewModelHelper<T, FilterPayload>)
  public type
    TFilterPredicate = reference to function(const Rec: T; const Filter: FilterPayload): Boolean;
  strict private
    FFilter: FilterPayload;
    FFilterPredicate: TFilterPredicate;
  public
    constructor Create(const FilterPredicate: TFilterPredicate);

    function Filter: FilterPayload;
    procedure SetFilter(const Filter: FilterPayload);

    function DecorateData(const Data: IList<T>): IList<T>;
  end;

  TSortedByColumnGridViewModelHelper<T: IObservable> = class(TDelegatedObservable, ISortedByColumnGridViewModelHelper<T>)
  strict private
    FComparison: TComparison<T>;
    FHeaderClickFunc: Func<TArray<TColumn>, TColumn, TComparisonByIndex<T>, TComparison<T>>;
    FGetColumns: Func<TArray<TColumn>>;
    FComparisonByIndex: TComparisonByIndex<T>;

    procedure GridHeaderClick(Column: TColumn);
  private
    procedure GridHeaderMove(Column: TColumn; FromIndex, ToIndex: Integer);
  public
    constructor Create(const Grid: TGrid; const ComparisonByIndex: TComparisonByIndex<T>);

    function OnSortByHeaderClick: THeaderClick;
    function OnSortByHeaderMove: TColumnMovedEvent;
    function DecorateData(const Data: IList<T>): IList<T>;
  end;

implementation

{ TAbstractReadonlyGridViewModel<T> }

constructor TAbstractReadonlyGridViewModel<T>.Create(
  const Grid: TGrid;
  const Data: IList<T>);
begin

  inherited Create(nil);

  FGetColumnIndexByCol := function(const Col: Integer): Integer
    begin
      Result := Grid.Columns[Col].OriginalIndex;
    end;

  var LSelf := Self;
  FUpdateGrid := procedure(const Data: IList<T>)
    begin
        Grid.BeginUpdate;
        try
          var O: IObservable;
          if Supports(FData, IObservable, O) then
            O.UnregisterObserver(LSelf);

          FData := Data;
          FViewData := DecorateData(Data);
          Grid.RowCount := FViewData.Count;

          if Supports(FData, IObservable, O) then
            O.RegisterObserver(LSelf);

        finally
          Grid.EndUpdate;
        end;
        Broadcast('Grid updated');
    end;

  FGetSelectedRow := function: Integer
    begin
      Result := Min(Grid.Row, Grid.RowCount - 1);
    end;

  FGetSelectedColumn := function: Integer
    begin
      Result := Grid.Col;
    end;

  SetData(Utilities.CheckNotNullAndSet(Data, 'Data'));
end;

function TAbstractReadonlyGridViewModel<T>.Data: IList<T>;
begin
  Result := FData;
end;

function TAbstractReadonlyGridViewModel<T>.DecorateData(
  const Data: IList<T>): IList<T>;
begin
  Result := Data;
end;

function TAbstractReadonlyGridViewModel<T>.GetColumnIndexByCol(const Row: Integer): Integer;
begin
  Result := FGetColumnIndexByCol(Row);
end;

procedure TAbstractReadonlyGridViewModel<T>.Notify(
  const Sender: IInterface;
  const Notification: INotification);
begin
  FUpdateGrid(FData);
end;

function TAbstractReadonlyGridViewModel<T>.OnGetFieldValue: TOnGetValue;
begin
  Result := GridGetValue;
end;

function TAbstractReadonlyGridViewModel<T>.SelectedColumn: Integer;
begin
  Result := FGetSelectedColumn();
end;

function TAbstractReadonlyGridViewModel<T>.SelectedItem: T;
begin
  Result := nil;
  var Row := SelectedRow;
  if (Row > -1) and (FViewData.Count > Row) then
    Result := FViewData[Row];
end;

function TAbstractReadonlyGridViewModel<T>.SelectedRow: Integer;
begin
  Result := FGetSelectedRow();
end;

procedure TAbstractReadonlyGridViewModel<T>.SetData(const Data: IList<T>);
begin
  FUpdateGrid(Data);
end;

function TAbstractReadonlyGridViewModel<T>.ViewData: IList<T>;
begin
  Result := FViewData;
end;

{ TAbstractWriteableGridView<T> }

function TAbstractWriteableGridView<T>.OnSetFieldValue: TOnSetValue;
begin
  Result := GridSetValue;
end;

{ TFilteredGridViewModelHelper<T, FilterPayload> }

constructor TFilteredGridViewModelHelper<T, FilterPayload>.Create(const FilterPredicate: TFilterPredicate);
begin
  inherited Create;

  FFilterPredicate := Utilities.CheckNotNullAndSet<TFilterPredicate>(FilterPredicate, 'FilterPredicate');
end;

function TFilteredGridViewModelHelper<T, FilterPayload>.DecorateData(const Data: IList<T>): IList<T>;
begin
  Result := TCollections.GetListOfDeepObservable<T>(Data.Where(function(const Rec: T): Boolean
    begin
      Result := FFilterPredicate(Rec, FFilter);
    end));
end;

function TFilteredGridViewModelHelper<T, FilterPayload>.Filter: FilterPayload;
begin
  Result := FFilter;
end;

procedure TFilteredGridViewModelHelper<T, FilterPayload>.SetFilter(const Filter: FilterPayload);
begin
  FFilter := Filter;
end;

{ TSortedByColumnGridViewModelHelper<T> }

constructor TSortedByColumnGridViewModelHelper<T>.Create(
  const Grid: TGrid;
  const ComparisonByIndex: TComparisonByIndex<T>);
begin
  inherited Create(nil);

  Utilities.CheckNotNullAndSet(Grid, 'Grid');

  FComparisonByIndex := Utilities.CheckNotNullAndSet<TComparisonByIndex<T>>(ComparisonByIndex, 'ComparisonByIndex');

  FGetColumns := function: TArray<TColumn>
    begin
      SetLength(Result, Grid.Model.ColumnCount);
      for var Index := 0 to Grid.Model.ColumnCount - 1 do
        Result[Index] := Grid.Model.Columns[Index];
    end;

  FHeaderClickFunc := function(const Columns: TArray<TColumn>; const Column: TColumn; const ComparisonByIndex: TComparisonByIndex<T>): TComparison<T>
    begin
      case TGridColumnSortType(Column.Tag) of
        TGridColumnSortType.Asc: Column.Tag := Integer(TGridColumnSortType.Desc);
        TGridColumnSortType.Desc: Column.Tag := Integer(TGridColumnSortType.None);
        TGridColumnSortType.None: Column.Tag := Integer(TGridColumnSortType.Asc);
      end;

      Result := function(const Left: T; const Right: T): Integer
        begin
          Result := 0;
          for var Index := Low(Columns) to High(Columns) do
          begin
            var SortType: TGridColumnSortType := TGridColumnSortType(Columns[Index].Tag);

            if SortType <> TGridColumnSortType.None then
            begin
              Result := ComparisonByIndex(Columns[Index].OriginalIndex, Left, Right);
              if SortType = TGridColumnSortType.Desc then
                Result := -Result;
              if Result <> 0 then
                Exit;
            end;
          end;
        end;
    end;

  FComparison := nil;
end;

function TSortedByColumnGridViewModelHelper<T>.DecorateData(const Data: IList<T>): IList<T>;
begin
  if not Assigned(FComparison) then
    Exit(Data);

  Result := TCollections.GetListOfDeepObservable<T>(Data);

  Result.Sort(FComparison);
end;

procedure TSortedByColumnGridViewModelHelper<T>.GridHeaderClick(Column: TColumn);
begin
  FComparison := FHeaderClickFunc(FGetColumns, Column, FComparisonByIndex);
  Broadcast('Sorted');
end;

procedure TSortedByColumnGridViewModelHelper<T>.GridHeaderMove(Column: TColumn; FromIndex, ToIndex: Integer);
begin
  GridHeaderClick(Column);
end;

function TSortedByColumnGridViewModelHelper<T>.OnSortByHeaderClick: THeaderClick;
begin
  Result := GridHeaderClick;
end;

function TSortedByColumnGridViewModelHelper<T>.OnSortByHeaderMove: TColumnMovedEvent;
begin
  Result := GridHeaderMove;
end;

end.

