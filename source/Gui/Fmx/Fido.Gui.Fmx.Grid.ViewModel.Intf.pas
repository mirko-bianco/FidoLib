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

unit Fido.Gui.Fmx.Grid.ViewModel.Intf;

interface

uses
  System.Generics.Defaults,
  FMX.Grid,

  Spring.Collections,

  Fido.Exceptions,
  Fido.Collections.DeepObservableList.Intf,
  Fido.DesignPatterns.Observable.Intf;

type
  // A grid view model that allows only showing the data
  IReadonlyGridViewModel<T: IObservable> = interface(IObservable)
    ['{92FBAE08-0A79-4B2E-9A49-CB1373F5D4F5}']

    function OnGetFieldValue: TOnGetValue;

    procedure SetData(const Data: IList<T>);
    function SelectedRow: Integer;
    function SelectedColumn: Integer;
    function SelectedItem: T;
  end;

  // A grid view model that allows also the direct editing of data in the grid
  IWriteableGridViewModel<T: IObservable> = interface(IReadonlyGridViewModel<T>)
    ['{FA4BDDA2-368E-4035-B3B7-D2BAFEB4E6C0}']

    function OnSetFieldValue: TOnSetValue;
  end;

  // Helpers are not grid model, but can be used by the grid view models to alter the shown data
  // by means of the DecorateData function.

  // An helper for grid view models that allows filtering of the data based on a specific payload
  IFilteredGridViewModelHelper<T: IObservable; FilterPayload> = interface(IInvokable)
    ['{91A90CF3-27AA-4B52-900B-59507CCDE53E}']

    function Filter: FilterPayload;
    procedure SetFilter(const Filter: FilterPayload);

    function DecorateData(const Data: IList<T>): IList<T>;
  end;

  TGridColumnSortType = (None, Asc, Desc);

  TComparisonByIndex<T> = reference to function(const Index: Integer; const Left: T; const Right: T): Integer;

  // An helper for grid view models that allows sorting of the data based on clicking and moving the grid column headers
  ISortedByColumnGridViewModelHelper<T: IObservable> = interface(IObservable)
    ['{93B214FD-2F8E-4D70-93C4-21951273B08A}']

    function OnSortByHeaderClick: THeaderClick;
    function OnSortByHeaderMove: TColumnMovedEvent;
    function DecorateData(const Data: IList<T>): IList<T>;
  end;

implementation

end.
