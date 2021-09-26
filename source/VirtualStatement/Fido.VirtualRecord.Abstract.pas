(*
 * Copyright 2021 Mirko Bianco (email: mirko.bianco.work@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

unit Fido.VirtualRecord.Abstract;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.Classes,
  Data.DB,

  Spring.Collections,

  Fido.Model.TypeConverter,
  Fido.VirtualStatement.Attributes,
  Fido.VirtualInterface,
  Fido.Types,
  Fido.VirtualQuery.Attributes;

type
  TAbstractVirtualRecord<T: IInterface> = class abstract (TVirtualInterface<T>)
  strict private const
    GetterPrefix = 'GET';
  strict private
    procedure DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);

    procedure ProcessRecordAttributes;
    procedure ProcessAttribute(const Attribute : TCustomAttribute; const MethDesc: TMethodDescriptor);
    procedure ProcessMethod(const Method: TRttiMethod; const Collection: IDictionary<string, TMethodDescriptor>);
    function GetIsGetterName(const Name: string): boolean;
    function GetMappedName(const Name: string): string;
  protected
    FRecordMethods: IDictionary<string, TMethodDescriptor>;
  public
    constructor Create; reintroduce;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  System.Generics.Collections;

{ TAbstractVirtualRecord<T> }

constructor TAbstractVirtualRecord<T>.Create;
begin
  inherited Create(DoInvoke);

  FRecordMethods := TCollections.CreateDictionary<string, TMethodDescriptor>([doOwnsValues]);
  ProcessRecordAttributes;
end;

procedure TAbstractVirtualRecord<T>.DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
var
  MethodDesc: TMethodDescriptor;
begin
  MethodDesc := FRecordMethods.GetValueOrDefault(Method.Name);

  // all methods should be cached and processed by now
  Assert(Assigned(MethodDesc) and (MethodDesc.Category in [mcColGetter]));

  case MethodDesc.Category of
    mcColGetter:
      begin
        Result := MethodDesc.Converter.GetFromVariant(MethodDesc.FieldValue);
      end;
  end;
end;

function TAbstractVirtualRecord<T>.GetIsGetterName(const Name: string): boolean;
begin
  Result := Name.StartsWith(GetterPrefix, true);
end;

function TAbstractVirtualRecord<T>.GetMappedName(const Name: string): string;
var
  Prefix: string;
begin
  // TODO use actual Maps
  Result := Name.ToUpper;

  // remove getter prefix; TODO setters?
  if GetIsGetterName(Result) then
    Delete(Result, 1, Length(GetterPrefix));
end;

procedure TAbstractVirtualRecord<T>.ProcessAttribute(const Attribute: TCustomAttribute; const MethDesc: TMethodDescriptor);
begin
  if (Attribute is ColumnAttribute) then
    MethDesc.MappedName := ColumnAttribute(Attribute).Line
end;

procedure TAbstractVirtualRecord<T>.ProcessMethod(const Method: TRttiMethod; const Collection: IDictionary<string, TMethodDescriptor>);
var
  Attribute : TCustomAttribute;
  MethodDesc: TMethodDescriptor;
  O: TDatasetOperation;
  S: string;
begin
  if not FRecordMethods.TryGetValue(Method.Name, MethodDesc) then
  begin
    Assert(Method.MethodKind in [mkFunction]);

    MethodDesc := TMethodDescriptor.Create;
    Collection.Add(Method.Name, MethodDesc);

    MethodDesc.Category := mcNone;
    MethodDesc.OriginalName := Method.Name;
    MethodDesc.Converter := nil;

    MethodDesc.Converter := DataTypeConverter.GetDescriptor(Method.ReturnType);

    MethodDesc.MappedName := GetMappedName(Method.Name);
  end;

  // auto describe based in attributes
  for Attribute in Method.GetAttributes do
    ProcessAttribute(Attribute, MethodDesc);
end;

procedure TAbstractVirtualRecord<T>.ProcessRecordAttributes;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Attribute: TCustomAttribute;
  Method: TRttiMethod;
  Pair: TPair<string, TMethodDescriptor>;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(TypeInfo(T));

    // process all methods (and their attributes)
    for Method in RttiType.GetMethods do
      ProcessMethod(Method, FRecordMethods);

    // set remaining methods to rows affected or colgetters
    for Pair in FRecordMethods do
      with Pair.Value do
        if Category = mcNone then
          Category := mcColGetter;
  finally
    Context.Free
  end;
end;

end.
