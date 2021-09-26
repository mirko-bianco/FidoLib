# Fido Core documentation

Fido Core contains several features that can help developers improve the quality of their code.

Below a list of the most important features:
- [Mappers](#mappers)
- [JSON marshalling and unmarshalling](#json-marshalling-and-unmarshalling)
- [Virtual database features](#virtual-database-features)
- [Client virtual Apis](#client-virtual-apis)
- [Server virtual Apis](#server-virtual-apis)

## Mappers
Unit: `Fido.Mappers`.

Mappers are useful when it is required to transform from one type to another.

##### Registration

The mappings are registered into the global `mappers` structure, and can be registered as following: 

```pascal
  Mappers.RegisterMapper<ISongRecord, ISong>(
    procedure(Source: ISongRecord; Destination: ISong)
    begin
      Destination.SetId(Source.Id);
      Destination.SetTitle(Source.Title);
    end);
```

##### Automapping

Please note that in the previous case the registration is redundant, given that properties and methods in `ISongRecord` and `ISong` are of the same type.

In case you need to migrate to and from types that have this similarities an auto mapping functionality will be used, adopting the following rules:

- Only published methods and properties are used
- Only functions with no parameters are used
- Only readable properties will be used to extract data
- Only writable properties will be used to inject data

##### Usage

The mappings can be used as such: 

```pascal
  try
    Mappers.Map<ISongRecord, ISong>(SongRecord, Song);
  except
    on E: Exception do
      Writeln(E.Message);
  end;
```

Both parameters of the `Map` method needs to be already instantiated.



## JSON marshalling and unmarshalling

Unit `Fido.Json.Marshalling`.

By using the `JSONUnmarshaller` and `JSONMarshaller` tools the developer can convert to and from JSON.

The following types are supported:

- Primitives (`string, Int64, Integer, Smallint, TDateTime, Extended, Double, Currency, Boolean, TGuid`).

- Nullable of primitives (`Spring.Nullable<T>` where T is one of the supported Primitives).

- Objects (any descendent of `TObject`). 

  Any published parameter-less function and readable property is marshalled.

  Any published one-parameter method and writeable property is unmarshalled.

- Interfaces (any descendent of `IInterface`). 

  Any public and published parameter-less function and readable property is marshalled.

  Any public and published one-parameter method and writeable property is unmarshalled.

- Lists of primitives (`Spring.Collections.IReadonlyList<T>` where T is one of the supported Primitives).
- Lists of interfaces (`Spring.Collections.IReadonlyList<T: IInterface>` ).

##### Virtual JSON implementation of interfaces

It is worth noting that when it comes to the unmarshalling to interfaces the developer doesn't need to provide an implementation for the interfaces.

Fido library will use the `TJSONVirtualDto` class to virtually implement the interface and expose the JSON values.

##### Aggregates

It is also worth noting that Fido library will process also aggregates and not only plain (as single level) JSON objects.

Let's say you have a JSON object as such:

```json
{
    "Id":1,
    "Guid":"F76CD4D4-35E1-4C66-A30A-37C050C0B324",
    "Title":"My Title",
    "IsGood":false,
    "ReleaseDate":"2021-09-16T16:59:07.096Z",
    "Price":"15.95",
    "Author":{
        "Id":2,
        "Name":"Author name"
    },
    "Years":[2001,2002],
    "Singers":[
        {
            "Id":3,
            "Name":"First singer"
        },
        {
            "Id":4,
            "Name":"Second singer"
        }
    ],
    "Media":2
}
```

then, you just provide an interface as such (again, **no implementation**):

```pascal
  ISinger = interface(IInvokable)
    ['{DC5320BB-1338-4745-B958-8A0BBD74DAB5}']
    function Id: Integer;
    function Name: string;
  end;

  IAuthor = interface(IInvokable)
    ['{2E7D194B-E2F4-4859-9330-8410164B4995}']

    function Id: Integer;
    function Name: string;
  end;

  ISong = interface(IInvokable)
    ['{8B681FE2-3C64-4D58-9DF6-0E9561B17E03}']

    function Id: Integer;
    function Guid: TGuid;
    function Title: string;
    function IsGood: Boolean;
    function ReleaseDate: Nullable<TDateTime>;
    function RetirementDate: Nullable<TDateTime>;
    function Price: Currency;

    function Author: IAuthor;

    function Years: IReadOnlyList<Integer>;
    function Singers: IReadOnlyList<ISinger>;

    function Media: TMediaType;
  end;
```

 and by calling:

```pascal
procedure UnmarshallSong(const SongJson: string);
var
  Song: ISong;
begin
  Song := JSONUnmarshaller.&To<ISong>(SongJson);
  
  ...
end;
```

You will have access to an interface instance that expressed the JSON. And that's what I mean when I say that the Fido library philosophy is "**describe behaviour instead of coding it, whenever is possible**".



##### Registration and overriding of the JSON mappings

The JSON mappings are registered into a global structure, and can be added or overridden by using the `MappingsUtilities`.

Let's say you don't want to encode your datetimes in ISO8601 (the standard mapping), but in another way, you can simply call:

```pascal
  uses
    Fido.JSON.Mapping;

  function DateTomMyFormat(const Value: TDateTime): string;
  begin
    // Your own implementation
  end;
  
  function TryMyFormatToDate(const Value: string; out DateTimeValue: TDateTime): Boolean;
  begin
    // Your own implementation
  end;

  MappingsUtilities.RegisterPrimitive<TDateTime>(
    function(const Value: TDateTime): string
    begin
      Result := DateToMyFormat(Value);
    end,
    function(const Value: string): TDateTime
    var
      DateTimeValue: TDateTime;
    begin
      Result := 0;
      if TryMyFormatToDate(Value, DateTimeValue) then
        Result := DateTimeValue
    end);
```

You can also override the default mapping for enumeratives (that by default uses the index) by calling:

```pascal
  uses
    Fido.JSON.Mapping;
    
  MappingsUtilities.RegisterEnumeratives(
    function(const Value: TValue): string
    begin
      // Whatever implementation you like
    end,
    function(const Value: string; const TypInfo: pTypeInfo): TValue
    begin
      // whatever implementation you like
    end);
```



##### Mapping classes and interfaces

Fido library will unmarshall classes and interfaces, adopting the following rules:

- Classes constructors must be parameter-less  
- Only procedures called `Set<NAME_OF_JSON_VALUE>` with one parameter are used
- Only writable properties will be used to inject data

Fido library will marshall interfaces, adopting the following rules:

- Only public and published parameter-less functions are used
- Only public and published readable properties will be used

Fido library will marshall classes, adopting the following rules:

- Only published parameter-less functions are used
- Only published readable properties will be used

##### Usage

The usage is straight forward, to convert from JSON to type:

```pascal
procedure UnmarshallSong(const SongJson: string);
var
  Song: ISong;
begin
  Song := JSONUnmarshaller.&To<ISong>(SongJson);
  
  ...
end;
```

And to convert from type to JSON:

```pascal
procedure MarshallSong(const Song: ISong);
var
  SongJson: string;
begin
  SongJson := JSONMarshaller.From<ISong>(Song);
  
  ...
end;  
```

## Virtual database features

The virtual database clients are interfaces that represent database statements (query, commands, sequences, stored procedures) and are enriched by means of attributes in order to allow the Fido library to work properly.

##### Connectivity

Fido library enables the connection to databases through implementations of the `IStatementExecutor` interface.
In FidoCore the FireDAC implementation is already available through the classes `TFireDacStatementExecutor` and `TFireDacConnections`.

The virtual database clients will use the `IStatementExecutor` internally to reach the database. All of this possible when registering the classes into the DI container (I said DI container, not global container...). 

##### Registration in the DI container

```pascal
var
  FireDacDatabaseParams: TStrings;
begin
  FireDacDatabaseParams := TStringList.Create;
  
  // Set FireDAC parameters
  ...
 
  Container.RegisterType<TFireDacConnections>.DelegateTo(
    function: TFireDacConnections
    begin
      Result := TFireDacConnections.Create(FireDacDatabaseParams);
    end).AsSingleton;
  Container.RegisterType<IStatementExecutor, TFireDacStatementExecutor>;
  
  ...
end;
```

##### SQL queries and commands as resources

Fido library treats the SQL queries and commands as resources, and as such you should provide those resources.

First of all you need to add the resources to the `dpr` file.

```pascal
{$R 'Queries.res' '<PATH_TO_THE_DOMAIN>\Queries.rc'}
```

Then create the `rc` file.

```pascal
#include "Song.Repository.Data.rcs"
```

Then create the `rcs` file.

```pascal
SQL_SONG_GetList RCDATA "<PATH_TO_THE_QUERIES>\SQL_SONG_GetList.sql"
SQL_SONG_Get RCDATA "<PATH_TO_THE_QUERIES>\SQL_SONG_Get.sql"
SQL_SONG_Update RCDATA "<PATH_TO_THE_QUERIES>\SQL_SONG_Update.sql"

```

The folder structure will end up like this.

```pascal
<DPR_LOCATION>\<PATH_TO_THE_DOMAIN>\<PATH_TO_THE_QUERIES>\sql files
```

The reason behind this structure  is that when you start working with a medium/large project and you have 100s of resources the  `dpr` file will become very slow when edited from the IDE.

### Virtual queries

Unit `Fido.VirtualQuery.Intf`.

With a virtual query you can abstract an SQL query and retrieve a `Spring.Collections.IReadonlyList<T: IInvokable>`.
You just need to declare the DTO (Data Transfer Object) interface and the query interface descendent from `IVirtualQuery` and  then use it.

##### Declaration

```pascal
ISongRecord = interface(IInvokable)
    ['{8B681FE2-3C64-4D58-9DF6-0E9561B17E03}']

    function Id: Integer;
    function Title: string;
  end;

  [SQLResource('SQL_SONG_GetList')]
  ISongListQuery = interface(IVirtualQuery)
    ['{731740A3-5B8E-46B2-9E96-53A366CB7B54}']

    function Open: IReadonlyList<ISongRecord>;
  end;
  
  [SQLResource('SQL_SONG_Get')]
  ISongByIdQuery = interface(IVirtualQuery)
    ['{0A3B473B-C3A1-4473-A553-8F094927182F}']

    function Open(const Id: Integer): IReadonlyList<ISongRecord>;
  end;
```

##### Attributes

Unit `Fido.VirtualQuery.Attributes`.

###### [SQLResource(RESOURCE_NAME)]

It sets the resource that needs to be used

###### [PagingLimit]

It sets the paging limit of the query, the number of records to return.

###### [PagingOffset]

It sets the paging Offset of the query, the number of records to skip.

##### Registrations

```pascal
  Containers.RegisterVirtualQuery<ISongRecord, ISongListQuery>(Container);
  Containers.RegisterVirtualQuery<ISongRecord, ISongByIdQuery>(Container);
```

##### Usage

```pascal
procedure DoSomethingWithSongListQuery(const SongListQuery: ISongListQuery);
var
  Item: ISongRecord;
begin
  for Item in SongListQuery.Open do
  begin
    //Do your magic here...
  end;
end;
```

### Virtual statements

Unit `Fido.VirtualStatement.Intf`.

With a virtual query you can abstract an SQL command or call to stored procedure or sequence.

A virtual statement can use a fully qualified DB object name (i.e. when a procedure or sequence is needed) or a resource.

##### Declaration

```pascal
  [Statement(stCommand, 'SQL_SONG_Update')]
  ISongUpdateByIdCommand = interface(IVirtualStatement)
    ['{8ACAA604-8182-4FE3-B205-BBE2153B87B8}']
    function Update(const Id: Integer; [Column('title')] const Name: string): Integer;
  end;
```

##### Attributes

unit `Fido.VirtualStatement.Attributes`.

###### [Statement(Type, RESOURCE_NAME)]

It sets statement type and the resource that needs to be used. Values can be: `stSequence, stFunction, stStoredProc, stCommand`.

For `stCommand` it is necessary to provide a resource.

For `stSequence, stFunction, stStoredProc` it is necessary to provide the fully qualified database object name.

###### [Execute]

It sets the decorated method as the method to run to execute the resource.

If the method is already called `Execute` or if the interface contains only one method, then the attribute is redundant.

###### [Column]

It sets the database name of a specific parameter.

i.e. in the previous declaration the second parameter of the `Update` method is called `Name` in the Delphi code, but `title` in the related resource.

###### [PagingLimit]

It sets the paging limit of the query, the number of records to return.

###### [PagingOffset]

It sets the paging Offset of the query, the number of records to skip.

##### Registrations

```pascal
  Containers.RegisterVirtualStatement<ISongUpdateByIdCommand>(Container);
```

##### Usage

```pascal
procedure TSongRepository.Update(const Song: ISong);
begin
  Guard.CheckNotNull(Song, 'Song');
  FSongUpdateByIdCommand.Update(Song.Id, Song.Title);
end;
```

### Virtual Api clients

Virtual Api clients are interfaces that represent services endpoints and are enriched by means of attributes in order to allow the Fido library to work properly.

Example of a virtual Api client:

```pascal
{$M+}   
IIdentityApi = interface(IClientVirtualApi)
  ['{6825ED01-3041-4528-8CA2-993DDBBCB9A7}']

  [Endpoint(rmPUT, '/account/{accountId}')]
  [RequestParam('Request')]
  function AccountByAccountIdPut(AccountId: Integer; Request: TUpdateIdentityRequest): TUpdateIdentityResponse;

  [Endpoint(rmPOST, '/accounts')]
  [RequestParam('Request')]
  function AccountsPost(Request: TCreateIdentityRequest): TCreateIdentityResponse;

  [Endpoint(rmDELETE, '/identity')]
  [RequestParam('Request')]
  procedure IdentityDelete(Request: TDeleteIdentityRequest);   
end;
```

A virtual Api client is always linked to an `IClientVirtualApiConfiguration` (that is injected into it) and it is able to extract information from it.
Every parameter-less function will be scanned in order to [resolve](#resolving-the-parameters) the call parameters.

#### Attributes

###### [EndPoint(Method, Endpoint)]
The **EndPoint** attribute defines what method and endpoint will be used during the call.
The EndPoint variable can contain [Mustache](https://mustache.github.io/) templates, that will be [resolved](#resolving-the-parameters) during the call.

Example:
```pascal
  [Endpoint(rmPUT, '/account/{accountId}')]
```

###### [Content(ContentType)]

The **Content** attribute is used to override the content of both request and response. By default the value is `application/json` so there's no need to define it if your service uses it.

Example:

```pascal
  [Content('application/json')]
```

###### [QueryParam(MethodParam, RestParam='')]

###### [HeaderParam(MethodParam, RestParam='')]

###### [FormParam(MethodParam, RestParam='')]

###### [FileParam(MethodParam, RestParam='')]

The **QueryParam**, **HeaderParam**, **FormParam** and **FileParam** attributes are used to define parameters that need to be set in the Query, Header, Form or File parameters.

Examples:

```pascal
  [HeaderParam('ApiKey')]
  [HeaderParam('ApiKey', 'Api-Key')]
```

The `MethodParam` is the name of the parameter as it can be found in the Delphi code (call parameter or Configuration function name), while the `RestParam` is the optional name of the parameter as is requested by the service. This is required usually only when, for example, the parameter name must be `something-somethingelse` and Delphi won't be able to find any parameter or method called that way (in case you are wondering why it is because Delphi do not allows to define names containing `-`).

###### [Request(MethodParam)]

The **Request** attribute is used when the call requires a JSON content, to inform the Fido library which parameter is to be converted in JSON and become the body of the request. 
If the body is an aggregate we strongly suggest to descend it from `TOwningObject` so that the whole hierarchy of children objects will be freed upon destruction of the aggregate.

Example

```pascal
  [RequestParam(''Request')]
```

###### [RestParam(MethodParam, RestParam='')]

The **RestParam** attribute is used when a configuration function name or a call parameter does not have the same name of the actual parameter the service needs.

Declaration Example:

```pascal
  [RestParam('Api-Key')]
```

Usage Examples:

```pascal
  IIdentityApi = interface(IVirtualRestApi)
    ['{6825ED01-3041-4528-8CA2-993DDBBCB9A7}']
    
    function AccountByAccountIdPut([RestParam('AccountId')] Account_Id: Integer; Request: TUpdateIdentityRequest): TUpdateIdentityResponse;
  end;
    
  IMySettings = interface(IBaseRestApiConfiguration)
    ['{D875BB18-F217-4208-8B41-17A867BA9F2B}']
    
    [RestParam('Api-Key')]
    function GetApiKey: string;
  end;
```

###### [ResponseHeaderParam(MethodParam, RestParam='')]

The **ResponseHeaderParam** attribute is used when a POST call is expected to return a header parameter when the response code is a specific number (usually 201), as explained [here](https://knpuniversity.com/screencast/rest/post). 
if ParamName is not set then it assumes it has the same value as HeaderParam.
The interface parameter must be out and of type string .

Declaration Example:

```pascal
  [ResponseHeaderParam(201, 'Location')]
```

Usage example:

```pascal
  IIdentityApi = interface(IVirtualRestApi)        
    ['{6825ED01-3041-4528-8CA2-993DDBBCB9A7}']        
    
    [ResponseHeaderParam(201, 'Location')]        
    procedure SomethingPost(const Request: TUpdateIdentityRequest; out Location: string);      
  end;
```

##### Resolving the parameters

Let's assume we have this Api definition:

```pascal
  {$M+}             
  IIdentityApi = interface(IVirtualRestApi)            
    ['{6825ED01-3041-4528-8CA2-993DDBBCB9A7}']                    
            
    [Endpoint(rmPUT, '/account/{accountId}')]            
    [RequestParam('Request')]            
    [HeaderParameter('ApiKey', 'Api-Key')]            
    function AccountByAccountIdPut([RestParam('AccountId')] Account_Id: Integer; Request: TUpdateIdentityRequest): TUpdateIdentityResponse;          
  end;                    
          
  IMySettings = interface(IBaseRestApiConfiguration)            
    ['{D875BB18-F217-4208-8B41-17A867BA9F2B}']            
    
    function GetApiKey: string;          
  end;
```

When the `AccountByAccountIdPut` method is called the Fido library executes the following steps:

 - Replaces the `{accountId}` in `/account/{accountId}` with the value of the `Account_Id` variable.
 - Converts the `Request` variable from a `TUpdateIdentityRequest` to a JSON and puts it as the request body.
 - Reads the result from the `GetApiKey` function and saves it as a header parameter called `Api-Key`.

And finally calls the REST service.

##### Out of the box and craftsmanship

The Fido library supports, out of the box, Apis based on JSON.
The unit `Fido.Api.Client.VirtualApi.json.pas` contains the class `TJSONClientVirtualApi<T: IClientVirtualApi; IConfiguration: IClientVirtualApiConfiguration>` that can be used to consume JSON based Apis.

If you need to support other flavours or you want to specialize the behaviour of the class please feel free to inherit from `TAbstractClientVirtualApi<T: IClientVirtualApi; IConfiguration: IClientVirtualApiConfiguration>` declared in `Fido.Api.Client.VirtualApi.Abstract.pas`.

The inherited class will implement the following methods:

```pascal
    function ConvertTValueToString(const Value: TValue): string; virtual; abstract;    
```

In this function you will convert a TValue into a string. The Virtual Api works with RTTI so it required a way to convert a parameter from TValue to a string (JSON and XML end up writing strings).

```pascal
    function ConvertResponseToDto(const Response: string; const ConvertToClass: TClass): TValue; virtual; abstract;
```

In this conversion you will convert from a string response  (JSON or XML depend on the Service implementation) to a instance.

```pascal
    function ConvertRequestDtoToString(const Value: TValue): string; virtual; abstract;
```

In this function you will convert the request from a TValue to a string.

```pascal
procedure CallApi(
  const Method: TRESTRequestMethod; 
  const Url: string; 
  const ContentType: string; 
  const QueryParams: IDictionary<string, string>; 
  const PostBody: string; 
  const HeaderParams: IDictionary<string, string>; 
  const FormParams: IDictionary<string, string>; 
  const FileParams: IDictionary<string, TStream>; 
  out ApiResponse: TRestApiResponse; 
  out ResponseHeaders: TStrings); virtual; abstract;
```

This is the core procedure that will perform the Api call. 
Is it important that you catch all the exceptions on this call because if you do not do that you will raise a generic exception, instead of getting the proper HTTP status code.

### Virtual Api servers

Virtual Apis servers classes allow you to create Api servers quickly. 

#### Servers

A server is the engine that will respond to incoming requests. Fido library provides already and Indy implementation, but feel free to implement your own, as long as it implements the `IApiServer` interface:

```pascal
  IApiServer = interface(IInvokable)
    ['{AA282BB3-418E-4835-8752-73D8DCCD326A}']

    function IsActive: Boolean;
    procedure SetActive(const Value: Boolean);

    procedure RegisterResource(const Resource: TObject);
    procedure RegisterWebSocket(const WebSocketClass: TClass);
    procedure RegisterRequestMiddleware(const Name: string; const Step: TRequestMiddlewareFunc);
    procedure RegisterResponseMiddleware(const Name: string; const Step: TResponseMiddlewareProc);
  end;
```

##### Usage

```pascal
program FileRestServerExample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  classes,
  System.SysUtils,
  System.IOUtils,
  Spring,
  Fido.Http.Types,
  Fido.Api.Server.Intf,
  Fido.Api.Server.Indy,
  Fido.Web.Server.Files,
  TestData in 'TestData.pas';

var
  RestServer: IApiServer;
begin
  ReportMemoryLeaksOnShutdown := True;
  RestServer := TIndyApiServer.Create(
    8080,
    50,
    TFileWebServer.Create(
      'public',
      'index.html'),
    TSSLCertData.CreateEmpty);
  try
    try
      RestServer.RegisterResource(TTestResource.Create);
      RestServer.SetActive(True);

      Readln;
    except
      on E: Exception do
      begin
        Writeln(E.ClassName, ': ', E.Message);
        Readln;
      end;
    end;

  finally
    RestServer.SetActive(False);
    RestServer := nil;
  end;
end.
```

#### Resources

Endpoints are added to the server by registering resources.

Example of a resource:

```pascal
unit TestData;

interface

uses
  Fido.Http.Types,
  Fido.Api.Server.Resource.Attributes;

type
  {$M+}
  TData = class
  private
    FName: string;
    FLastName: string;
    FAge: Integer;
  published
    property Name: string read FName write FName;
    property LastName: string read FLastName write FLastName;
    property Age: Integer read FAge write FAge;
  end;

  {$M+}
  [BaseUrl('/Test')]
  [Consumes(mtJson)]
  [Produces(mtJson)]
  TTestResource = class(TObject)

    [Path(rmGet, '/helloworld')]
    function HelloWorld: string;

    [Path(rmGet, '/helloworld/{name}')]
    function HelloWorldName(const [PathParam]Name: string): string;

    [Path(rmGet, '/Token')]
    procedure GetToken(out [HeaderParam] Token: string);

    [Path(rmPost, '/PostData')]
    procedure PostData(const [BodyParam] Data: TData);

    [Path(rmPost, '/OutData')]
    procedure OutData(out [BodyParam] Data: TData);

    [Path(rmGet, '/OutData')]
    function Data: TData;
  end;

implementation

{ TTestResource }

function TTestResource.Data: TData;
begin
  Data := TData.Create;
  Data.Name := 'Me';
  Data.LastName := 'again me';
  Data.Age := 22;
end;

procedure TTestResource.GetToken(out Token: string);
begin
  Token := '{C6A68632-28E3-410F-8B6C-F8EB7133AF8B}';
end;

function TTestResource.HelloWorld: string;
begin
  Result := 'Hello World';
end;

function TTestResource.HelloWorldName(const Name: string): string;
begin
  Result := 'Hello World and ' + Name;
end;

procedure TTestResource.OutData(out Data: TData);
begin
  Data := TData.Create;
  Data.Name := 'Me';
  Data.LastName := 'again me';
  Data.Age := 12;
end;

procedure TTestResource.PostData(const Data: TData);
begin
  WriteLn(Data.Name);
  WriteLn(Data.LastName);
  WriteLn(Data.Age);
end;

end.
```

Resources are decorated with attributes so that the server can understand how to process it.

#### Attributes

###### [BaseUrl(URL)]

The **BaseUrl** attribute defines the endpoint base URL.

Example:

```pascal
  [BaseUrl('/api')]
```

###### [Consumes(MimeType)]

The **Consumes** attribute defines what mime type the endpoint consumes. 

Example:

```pascal
  [Consumes(mtJson)]
```

###### [Produces(MimeType)]

The **Produces** attribute defines what mime type the endpoint produces. 

Example:

```pascal
  [Produces(mtJson)]
```

###### [Path(Method, Path)]

The **Path** attribute defines to what method and endpoint the decorated method will respond.
The Path variable can contain [Mustache](https://mustache.github.io/) templates, that will be [resolved](#resolving-the-parameters) during the call.

Example:

```pascal
  [Endpoint(rmPUT, '/account/{accountId}')]
```

###### [ResponseCode(Code, Text)]

The **ResponseCode** attribute is used to override the standard response (200, 'OK') that the Server will apply to the response.

Example:

```pascal
  [ResponseCode(201, 'something')]
```

###### [QueryParam(MethodParam, RestParam='')]

###### [HeaderParam(MethodParam, RestParam='')]

###### [FormParam(MethodParam, RestParam='')]

###### [PathParam(MethodParam, RestParam='')]

###### [BodyParam(MethodParam, RestParam='')]

The **QueryParam**, **HeaderParam**, **FormParam**, **PathParam** and **BodyParam** attributes are used to define parameters that need to be set in or retrieved from the Query, Header, Form, Path parameters or the body.

Examples:

```pascal
  [HeaderParam('ApiKey')]
  [HeaderParam('ApiKey', 'Api-Key')]
```

The `MethodParam` is the name of the parameter as it can be found in the Delphi code (call parameter or Configuration function name), while the `RestParam` is the optional name of the parameter as is requested by the service. This is required usually only when, for example, the parameter name must be `something-somethingelse` and Delphi won't be able to find any parameter or method called that way (in case you are wondering why it is because Delphi do not allows to define names containing `-`).

###### [WebSocketPath(Path)]

The **WebSocketPath** attribute is used to instruct the server to which path the websocket will respond to.

Example:

```pascal
  [WebSocketPath('/')]
```

###### [RequestMiddleware(Name)]

The **RequestMiddleware** attribute is used to instruct the server that the decorated method will use the named middleware on the request before the method itself is called.

Example:

```pascal
  [RequestMiddleware('First decorator')]
```

###### [ResponseMiddleware(Name)]

The **ResponseMiddleware** attribute is used to instruct the server that the decorated method will use the named middleware on the response before the method itself is called.

Example:

```pascal
  [ResponseMiddleware('Last decorator')]
```
