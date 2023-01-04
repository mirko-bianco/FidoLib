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

unit Fido.Exceptions.Marshalling;

interface

uses
  System.SysUtils,
  System.JSON,
  System.TypInfo,
  System.Rtti,

  Spring,
  Spring.Collections,

  Fido.JSON.Mapping,
  Fido.JSON.Marshalling;

implementation

initialization
  MappingsUtilities.RegisterType<Exception>(
    function(const Value: Exception): string
    var
      JSONObject: IShared<TJSONObject>;
      RttiContext: TRttiContext;
      RttiType: TRttiType;
    begin
      JSONObject := Shared.Make(TJSONObject.Create);
      if not Value.Message.IsEmpty then
        JSONObject.AddPair('Message', Value.Message);
      if not Value.StackTrace.IsEmpty then
        JSONObject.AddPair('StackTrace', Value.StackTrace);
      if Assigned(Value.InnerException) then
        JSONObject.AddPair('InnerException', JSONMarshaller.From(Value.InnerException));

      RttiContext := TRttiContext.Create;
      RttiType := RttiContext.GetType(Value.ClassInfo);

      TCollections.CreateList<TRttiMethod>(RttiType.GetMethods)
        .Where(function(const Method: TRttiMethod): Boolean
          begin
            Result := (Method.Visibility in [mvPublished]) and (Method.MethodKind = mkFunction) and (Length(Method.GetParameters) = 0);
          end)
        .ForEach(procedure(const Method: TRttiMethod)
          var
            ReturnValue: TValue;
            MarshalledValue: string;
          begin
            ReturnValue := Method.Invoke(Value, []);
            try
              MarshalledValue := JSONMarshaller.From(ReturnValue, Method.ReturnType.Handle);
              if not MarshalledValue.IsEmpty then
                JSONObject.AddPair(TJsonPair.Create(Method.Name, TJSONValue.ParseJSONValue(MarshalledValue)));
            except
            end;
          end);

      TCollections.CreateList<TRttiProperty>(RttiType.GetProperties)
        .Where(function(const Prop: TRttiProperty): Boolean
          begin
            Result := (Prop.Visibility in [mvPublished]) and Prop.IsReadable;
          end)
        .ForEach(procedure(const Prop: TRttiProperty)
          var
            ReturnValue: TValue;
            MarshalledValue: string;
          begin
            ReturnValue := Prop.GetValue(Value);
            try
              MarshalledValue := JSONMarshaller.From(ReturnValue, Prop.PropertyType.Handle);
              if not MarshalledValue.IsEmpty then
                JSONObject.AddPair(TJsonPair.Create(Prop.Name, TJSONValue.ParseJSONValue(MarshalledValue)));
            except
            end;
          end);

      Result := JSONObject.ToJSON;
    end,
    function(const Value: string; const TypInfo: pTypeInfo): Exception
    begin
      Result := nil; //Not supposed to be used
    end);

end.
