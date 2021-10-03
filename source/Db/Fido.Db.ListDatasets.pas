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

unit Fido.Db.ListDatasets;

interface

uses
  System.SysUtils,
  Data.DB,

  Spring,
  Spring.Collections,

  Fido.DesignPatterns.Observable.Intf,
  Fido.Collections.DeepObservableList.Intf,
  Fido.Db.DatasetFieldAttributes.Intf,
  Fido.Db.DatasetFieldAttributes,
  Fido.Db.ListDataset;

type
  ListDatasets = record
    // Transforms an IList<T> into a TDataSet
    class function ListToDataset<TEntity: IInvokable>(
      const List: IList<TEntity>;
      const EntityFactoryFunc: TFunc<PTypeInfo, TValue>): TDataSet; overload; static;

    // Transforms an IList<T> into a TDataSet.
    // Accepts an IDatasetFieldAttributes in order to decorate the fields
    class function ListToDataset<TEntity: IInvokable>(
      const List: IList<TEntity>;
      const EntityFactoryFunc: TFunc<PTypeInfo, TValue>;
      const DatasetFieldAttributes: IDatasetFieldAttributes): TDataSet; overload; static;

    // Transforms an IList<T> into a read only TDataSet.
    class function ListToReadOnlyDataset<TEntity: IInvokable>(const List: IList<TEntity>): TDataSet; overload; static;

    // Transforms an IList<T> into a read only TDataSet.
    // Accepts an IDatasetFieldAttributes in order to decorate the fields
    class function ListToReadOnlyDataset<TEntity: IInvokable>(
      const List: IList<TEntity>;
      const DatasetFieldAttributes: IDatasetFieldAttributes): TDataSet; overload; static;

    // Transforms an observable Entity into a TDataSet.
    class function ObservableEntityToDataset<TEntity: IObservable>(const Entity: TEntity): TDataSet; overload; static;

    // Transforms a list of observable Entities into a TDataSet.
    class function ListOfObservablesToDataset<TEntity: IObservable>(const List: IDeepObservableList<TEntity>): TDataSet; overload; static;

    // Transforms an Entity into a TDataSet.
    class function EntityToDataset<TEntity: IInvokable>(const Entity: TEntity): TDataSet; overload; static;

    // Transforms an observable Entity into a TDataSet.
    // Accepts an IDatasetFieldAttributes in order to decorate the fields
    class function ObservableEntityToDataset<TEntity: IObservable>(
      const Entity: TEntity;
      const DatasetFieldAttributes: IDatasetFieldAttributes): TDataSet; overload; static;

    // Transforms a list of observable Entities into a TDataSet.
    // Accepts an IDatasetFieldAttributes in order to decorate the fields
    class function ListOfObservablesToDataset<TEntity: IObservable>(
      const List: IDeepObservableList<TEntity>;
      const DatasetFieldAttributes: IDatasetFieldAttributes): TDataSet; overload; static;

    // Transforms an Entity into a TDataSet.
    // Accepts an IDatasetFieldAttributes in order to decorate the fields
    class function EntityToDataset<TEntity: IInvokable>(
      const Entity: TEntity;
      const DatasetFieldAttributes: IDatasetFieldAttributes): TDataSet; overload; static;
  end;

implementation

class function ListDatasets.ObservableEntityToDataset<TEntity>(const Entity: TEntity): TDataSet;
begin
  Result := ObservableEntityToDataset<TEntity>(Entity, TDatasetFieldAttributes.Create);
end;

class function ListDatasets.ObservableEntityToDataset<TEntity>(const Entity: TEntity; const DatasetFieldAttributes: IDatasetFieldAttributes): TDataSet;
var
  ListDataSet: TListDataSetOfObservable<TEntity>;
begin
  ListDataSet := TListDataSetOfObservable<TEntity>.Create(
    nil,
    function(TypeInfo: PTypeInfo): TValue
    begin
    end,
    DatasetFieldAttributes);
  ListDataSet.DataList.Add(Entity);
  ListDataSet.Open;

  Result := ListDataSet;
end;

class function ListDatasets.ListToReadOnlyDataset<TEntity>(const List: IList<TEntity>; const DatasetFieldAttributes: IDatasetFieldAttributes): TDataSet;
var
  ListDataSet: TListDataSet<TEntity>;
begin
  ListDataSet := TListDataSet<TEntity>.Create(
    nil,
    function(TypeInfo: PTypeInfo): TValue
    begin
    end,
    DatasetFieldAttributes);
  ListDataSet.DataList := List;
  ListDataSet.ReadOnly := True;
  ListDataSet.Open;

  Result := ListDataSet;
end;

class function ListDatasets.ListToReadOnlyDataset<TEntity>(const List: IList<TEntity>): TDataSet;
begin
  Result := ListToReadOnlyDataset<TEntity>(List, TDatasetFieldAttributes.Create);
end;

class function ListDatasets.EntityToDataset<TEntity>(
  const Entity: TEntity): TDataSet;
begin
  Result := EntityToDataset<TEntity>(Entity, TDatasetFieldAttributes.Create);
end;

class function ListDatasets.EntityToDataset<TEntity>(const Entity: TEntity;
  const DatasetFieldAttributes: IDatasetFieldAttributes): TDataSet;
var
  ListDataSet: TListDataSet<TEntity>;
begin
  ListDataSet := TListDataSet<TEntity>.Create(
    nil,
    function(TypeInfo: PTypeInfo): TValue
    begin
    end,
    DatasetFieldAttributes);
  ListDataSet.DataList.Add(Entity);
  ListDataSet.Open;

  Result := ListDataSet;
end;

class function ListDatasets.ListOfObservablesToDataset<TEntity>(
  const List: IDeepObservableList<TEntity>;
  const DatasetFieldAttributes: IDatasetFieldAttributes): TDataSet;
var
  ListDataSet: TListDataSetOfObservable<TEntity>;
begin
  ListDataSet := TListDataSetOfObservable<TEntity>.Create(
    nil,
    function(TypeInfo: PTypeInfo): TValue
    begin
    end,
    DatasetFieldAttributes);
  ListDataSet.DataList := List;
  ListDataSet.Open;

  Result := ListDataSet;
end;

class function ListDatasets.ListOfObservablesToDataset<TEntity>(
  const List: IDeepObservableList<TEntity>): TDataSet;
begin
  Result := ListOfObservablesToDataset<TEntity>(List, TDatasetFieldAttributes.Create);
end;

class function ListDatasets.ListToDataset<TEntity>(
  const List: IList<TEntity>;
  const EntityFactoryFunc: TFunc<PTypeInfo, TValue>;
  const DatasetFieldAttributes: IDatasetFieldAttributes): TDataSet;
var
  ListDataSet: TListDataSet<TEntity>;
begin
  ListDataSet := TListDataSet<TEntity>.Create(
    nil,
    EntityFactoryFunc,
    DatasetFieldAttributes);
  ListDataSet.DataList := List;
  ListDataSet.Open;

  Result := ListDataSet;
end;

class function ListDatasets.ListToDataset<TEntity>(
  const List: IList<TEntity>;
  const EntityFactoryFunc: TFunc<PTypeInfo, TValue>): TDataSet;
begin
  Result := ListToDataset<TEntity>(List, EntityFactoryFunc, TDatasetFieldAttributes.Create);
end;

end.
