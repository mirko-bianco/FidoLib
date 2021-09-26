unit Fido.Api.Client.VirtualApi.Json.Base.Test;

interface
uses
  System.Rtti,
  System.TypInfo,

  Fido.Api.Client.VirtualApi.Configuration.Intf,
  Fido.Api.Client.VirtualApi.Intf,
  Fido.Api.Client.VirtualApi.json;

type
  TBaseTestClientVirtualApi<T: IClientVirtualApi; IConfiguration: IClientVirtualApiConfiguration> = class(TJSONClientVirtualApi<T, IConfiguration>)
  public
    function ExposedConvertTValueToString(const Value: TValue): string;
    function ExposedConvertResponseToDto(const Response: string; const TypeInfo: PTypeInfo): TValue;
    function ExposedConvertRequestDtoToString(const Value: TValue): string;
  end;
 {$ENDREGION}

implementation

{ TBaseTestClientVirtualApi<T, IConfiguration> }

function TBaseTestClientVirtualApi<T, IConfiguration>.ExposedConvertRequestDtoToString(const Value: TValue): string;
begin
  Result := ConvertRequestDtoToString(Value);
end;

function TBaseTestClientVirtualApi<T, IConfiguration>.ExposedConvertResponseToDto(const Response: string; const TypeInfo: PTypeInfo): TValue;
begin
  Result := ConvertResponseToDto(Response, TypeInfo);
end;

function TBaseTestClientVirtualApi<T, IConfiguration>.ExposedConvertTValueToString(const Value: TValue): string;
begin
  Result := ConvertTValueToString(Value);
end;
{$ENDREGION}

end.
