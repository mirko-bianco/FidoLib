# Fido Core documentation

Fido Core contains several features that can help developers improve the quality of their code.

Below is a list of the most important features:
- [Mappers](#mappers)
- [JSON marshalling and unmarshalling](#json-marshalling-and-unmarshalling)
- [Virtual database features](#virtual-database-features)
- [Virtual Api clients](#virtual-api-clients)
- [Virtual Api servers](#virtual-api-servers)
- [Boxes](#boxes)
- [Async procedures](#async-procedures)
- [Async functions](#async-functions)
- [Signals and slots](#signals-and-slots)

## Mappers
Unit: `Fido.Mappers`.

Mappers are useful when it is required to transform from one type to another.

##### Registration

The mappings are registered into the global `mappers` structure and can be registered as following: 

```pascal
  Mappers.RegisterMapper<ISongRecord, ISong>(
    procedure(Source: ISongRecord; Destination: ISong)
    begin
      Destination.SetId(Source.Id);
      Destination.SetTitle(Source.Title);
    end);
```

##### Automapping

Please note that in the previous case, the registration is redundant, given that properties and methods in `ISongRecord` and `ISong` are of the same type.

In case you need to migrate to and from types that have these similarities, an auto-mapping functionality will be used, adopting the following rules:

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

By using the `JSONUnmarshaller` and `JSONMarshaller` tools, the developer can convert to and from JSON.

The following types are supported:

- Primitives (`string, Int64, Integer, Smallint, TDateTime, Extended, Double, Currency, Boolean, TGuid`).

- Nullable of primitives (`Spring.Nullable<T>` where T is one of the supported Primitives).

- Objects (any descendent of `TObject`). 

  Any published parameter-less function and readable property are marshalled.

  Any published one-parameter method and writeable property are unmarshalled.

- Records (other than `Spring.Nullable<T>` )

  Any public field, public parameter-less function and readable property are marshalled.

  Any public field, public parameter-less function and readable property are unmarshalled.

- Interfaces (any descendent of `IInterface`). 

  Any public parameter-less function and readable property are marshalled.

  Any public one-parameter method and writeable property are unmarshalled.

- Lists of primitives (`Spring.Collections.IReadonlyList<T>` where T is one of the supported Primitives).

- Lists of interfaces (`Spring.Collections.IReadonlyList<T: IInterface>` ).

##### Virtual JSON implementation of interfaces

It is worth noting that when it comes to the unmarshalling of interfaces, the developer doesn't need to implement the interfaces.

Fido library will use the `TJSONVirtualDto` class to implement the interface virtually and expose the JSON values.

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

You will have access to an interface instance that expresses the JSON. And that's what I mean when I say that the Fido library philosophy is "**describe behaviour instead of coding it, whenever is possible**".



##### Registration and overriding of the JSON mappings

The JSON mappings are registered into a global structure and can be added or overridden using the `MappingsUtilities`.

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

Fido library will unmarshal classes and interfaces, adopting the following rules:

- Classes constructors must be parameter-less  
- Only procedures called `Set<NAME_OF_JSON_VALUE>` with one parameter are used
- Only writable properties will be used to inject data

Fido library will marshall interfaces, adopting the following rules:

- Only public parameter-less functions are used
- Only public readable properties will be used

Fido library will marshall classes, adopting the following rules:

- Only published parameter-less functions are used
- Only published readable properties will be used

##### Usage

The usage is straightforward, to convert from JSON to type:

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

##### Multiple mapping configurations

FidoLib registers for you standard primitive type conversions rules, but you could have the need for supporting multiple configurations. 

Let's say you connect to two separate APIs, and each of them has a different date and time format. In this case, it is enough to register mappings with the  configuration name, for example:

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
    end,
    'MySpecialConfiguration');
```



 And the usage is again straightforward, to convert from JSON to type:

```pascal
procedure UnmarshallSong(const SongJson: string);
var
  Song: ISong;
begin
  Song := JSONUnmarshaller.&To<ISong>(SongJson, 'MySpecialConfiguration');
  
  ...
end;
```

And to convert from type to JSON:

```pascal
procedure MarshallSong(const Song: ISong);
var
  SongJson: string;
begin
  SongJson := JSONMarshaller.From<ISong>(Song, 'MySpecialConfiguration');
  
  ...
end;  
```

## Virtual database features

The virtual database clients are interfaces that represent database statements (query, commands, sequences, stored procedures) and are enriched using attributes to allow the Fido library to work properly.

##### Connectivity

Fido library enables the connection to databases through implementations of the `IStatementExecutor` interface.
In FidoCore the FireDAC implementation is already available through the classes `TFireDacStatementExecutor` and `TFireDacConnections`.

The virtual database clients will use the `IStatementExecutor` internally to reach the database. All of this is possible when registering the classes into the DI container (I said DI container, not global container...). 

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
      Result := TFireDacPerThreadConnections.Create(FireDacDatabaseParams);
    end).AsSingleton;
  Container.RegisterType<IStatementExecutor, TFireDacStatementExecutor>;
  
  ...
end;
```

##### SQL queries and commands as resources

Fido library treats the SQL queries and commands as resources, and as such, you should provide those resources.

First of all, you need to add the resources to the `dpr` file.

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

This structure is because when you start working with a medium/large project and have 100s of resources, the `dpr` file will become very slow when edited from the IDE.

### Virtual queries

Unit `Fido.VirtualQuery.Intf`.

With a virtual query, you can abstract an SQL query and retrieve a `Spring.Collections.IReadonlyList<T: IInvokable>`.
You just need to declare the DTO (Data Transfer Object) interface, and the query interface descendent from `IVirtualQuery` and  then use it.

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

With a virtual query, you can abstract an SQL command or call to stored procedure or sequence.

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

It sets the statement type and the resource that needs to be used. Values can be: `stSequence, stFunction, stStoredProc, stCommand`.

For `stCommand` it is necessary to provide a resource.

For `stSequence, stFunction, stStoredProc`, it is necessary to provide the fully qualified database object name.

###### [Execute]

It sets the decorated method as the method to run to execute the resource.

If the method is already called `Execute` or if the interface contains only one method, then the attribute is redundant.

###### [Column]

It sets the database name of a specific parameter.

i.e. in the previous declaration, the second parameter of the `Update` method is called `Name` in the Delphi code, but `title` in the related resource.

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

##### Using multiple databases

If you need to connect to multiple databases in your application, just register the `TXXXConnections`' and the `IStatementExecutor`s using  service  names and then register the virtual statements and virtual query using the appropriate service name.

```pascal
var
  FireDacDatabaseParamsFirstDatabase: TStrings;
  FireDacDatabaseParamsSecondDatabase: TStrings;
begin
  FireDacDatabaseParamsFirstDatabase := TStringList.Create;
  FireDacDatabaseParamsSecondDatabase := TStringList.Create;
  
  // Set FireDAC parameters
  ...
 
  Container.RegisterType<TFireDacConnections>('First_Database').DelegateTo(
    function: TFireDacConnections
    begin
      Result := TFireDacPerThreadConnections.Create(FireDacDatabaseParamsFirstDatabase);
    end).AsSingleton;
  Container.RegisterType<IStatementExecutor>('First_Database_Connector').DelegateTo(
    function: IStatementExecutor
    begin
      Result := TFireDacStatementExecutor.Create(Container.Resolve<TFireDacConnections>(First_Database));
    end);
    
   Container.RegisterType<TFireDacConnections>('Second_Database').DelegateTo(
    function: TFireDacConnections
    begin
      Result := TFireDacPerThreadConnections.Create(FireDacDatabaseParamsSecondDatabase);
    end).AsSingleton;
  Container.RegisterType<IStatementExecutor>('Second_Database_Connector').DelegateTo(
    function: IStatementExecutor
    begin
      Result := TFireDacStatementExecutor.Create(Container.Resolve<TFireDacConnections>(First_Database));
    end);
  
  ...
  
  Containers.RegisterVirtualQuery<ISongRecord, ISongListQuery>(Container, 'First_Database_Connector');
  Containers.RegisterVirtualStatement<ISongUpdateByIdCommand>(Container, 'Second_Database_Connector');
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

A virtual Api client is always linked to an `IClientVirtualApiConfiguration` (that is injected into it), and it is able to extract information from it.
Every parameter-less function will be scanned in order to [resolve](#resolving-the-parameters) the call parameters.

#### Attributes

###### [EndPoint(Method, Endpoint)]
The **EndPoint** attribute defines what method and endpoint will be used during the call.
The EndPoint variable can contain [Mustache](https://mustache.github.io/) templates that will be [resolved](#resolving-the-parameters) during the call.

Example:
```pascal
  [Endpoint(rmPUT, '/account/{accountId}')]
```

###### [Content(ContentType)]

The **Content** attribute is used to override the content of both request and response. By default, the value is `application/json`, so there's no need to define it if your service uses it.

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

The `MethodParam` is the name of the parameter as it can be found in the Delphi code (call parameter or Configuration function name), while the `RestParam` is the optional name of the parameter as is requested by the service. This is required usually only when, for example, the parameter name must be `something-somethingelse`, and Delphi won't be able to find any parameter or method called that way (in case you are wondering why it is because Delphi does not allow to define names containing `-`).

###### [Request(MethodParam)]

The **Request** attribute is used when the call requires JSON content to inform the Fido library which parameter is to be converted in JSON and become the body of the request. 
If the body is an aggregate, we strongly suggest descending it from `TOwningObject` so that the whole hierarchy of children objects will be freed upon the destruction of the aggregate.

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

If ParamName is not set then it assumes it has the same value as HeaderParam.
The interface parameter must be out and of type string.

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

If you need to support other flavours or you want to specialize the behaviour of the class, please feel free to inherit from `TAbstractClientVirtualApi<T: IClientVirtualApi; IConfiguration: IClientVirtualApiConfiguration>` declared in `Fido.Api.Client.VirtualApi.Abstract.pas`.

The inherited class will implement the following methods:

```pascal
    function ConvertTValueToString(const Value: TValue): string; virtual; abstract;    
```

In this function you will convert a TValue into a string. The Virtual Api works with RTTI, so it required a way to convert a parameter from TValue to a string (JSON and XML end up writing strings).

```pascal
    function ConvertResponseToDto(const Response: string; const ConvertToClass: TClass): TValue; virtual; abstract;
```

In this conversion, you will convert from a string response  (JSON or XML depending on the Service implementation) to a instance.

```pascal
    function ConvertRequestDtoToString(const Value: TValue): string; virtual; abstract;
```

In this function, you will convert the request from a TValue to a string.

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
Is it important that you catch all the exceptions on this call because if you do not do that, you will raise a generic exception instead of getting the proper HTTP status code.

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

Resources are decorated with attributes so that the server can understand how to process them.

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
The Path variable can contain [Mustache](https://mustache.github.io/) templates that will be [resolved](#resolving-the-parameters) during the call.

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

The `MethodParam` is the name of the parameter as it can be found in the Delphi code (call parameter or Configuration function name), while the `RestParam` is the optional name of the parameter as is requested by the service. This is required usually only when, for example, the parameter name must be `something-somethingelse`, and Delphi won't be able to find any parameter or method called that way (in case you are wondering why, it is because Delphi does not allow to define names containing `-`).

###### [WebSocketPath(Path)]

The **WebSocketPath** attribute is used to instruct the server to which path the websocket will respond to.

Example:

```pascal
  [WebSocketPath('/')]
```

###### [RequestMiddleware(Name, CommaSeparatedParams = '')]

The **RequestMiddleware** attribute is used to instruct the server that the decorated method will use the named middleware on the request before the method itself is called.

Example:

```pascal
  [RequestMiddleware('First decorator')]
```

###### [ResponseMiddleware(Name, CommaSeparatedParams = '')]

The **ResponseMiddleware** attribute is used to instruct the server that the decorated method will use the named middleware on the response before the method itself is called.

Example:

```pascal
  [ResponseMiddleware('Last decorator')]
```



### Boxes

Unit `Fido.Boxes`.

Boxes are, well, boxes...

Did you ever need to share a state between two or more pieces of code (possibly over different threads)?

 You can achieve that in FidoLib using `IBox<T>` and `IReadonlyBox<T>`.

##### Usage

```pascal
procedure Example;
var
  Box: IBox<Boolean>;
  ROBox: IReadonlyBox<Boolean>;
  Updater: TBoxUpdater<Boolean>;
begin
  Box := Box<Boolean>.Setup(True);

  TTask.Run(
    procedure
    begin
      WriteLn(Box.Value); // This would be True
      Box.UpdateValue(False);
    end).Wait;

  WriteLn(Box.Value); //This would be False;

  Box := Box<Boolean>.Setup(True, Updater);

  TTask.Run(
    procedure
    begin
      WriteLn(Box.Value); // This would be True
      Updater(False);
    end).Wait;

  WriteLn(Box.Value); //This would be False;
end;

```



### Async procedures

Unit `Fido.Async.Procs`.

Async procedures let you run a procedure or a sequence of procedures in a separate thread. You can use three different approaches:

- *fire and forget*. A separate task is created and executed without the caller having any control of how and when it will terminate.
- *fire and get ITask*. A separate task is created and executed and the caller gets an `ITask` that the caller can use.
- *fire and wait*. A separate task is created and executed, and the caller waits for the `Resolve` method to finish to know the final status of the async procedure.

##### Usage

```pascal
var
  AsyncProc: IAsyncProc;
  Task: ITask;
  FinalStatus: TAsyncProcStatus;
begin
  AsyncProc := AsyncProcs.
    Queue(
      procedure
      begin
        //First step
      end).
    &Then(
      procedure
      begin
        //Second step
      end). 
    &Then(
      procedure
      begin
        //Third step
      end).
    Within(100, //Ms
      procedure
      begin
        //What to do if it expires
      end).
    Catch(
      procedure(const E: Exception)
      begin
        //What to do in case of exception
      end);

  AsyncProc.Run; //Fire and forget
  
  AsyncProc.Run.Task; //Fire and get ITask
  
  FinalStatus := AsyncProc.Run.Resolve; //Fire and wait
end;
```



### Async functions

Unit `Fido.Async.Funcs`.

Async functions let you run a function or a sequence of functions in a separate thread. You can use three different approaches:

- *fire and forget*. A separate task is created and executed without the caller having any control of how and when it will terminate.
- *fire and get ITask*. A separate task is created and executed and the caller gets an `ITask` that the caller can use.
- *fire and wait*. A separate task is created, and executed and the caller waits for the `Resolve` method to finish to know the final status and value of the async function.

##### Usage

```pascal
var
  AsyncFunc: IAsyncFunc<Integer, string>;
  Future: ITask;
  FuncResult: TAsyncFuncResult<string>;
begin
  AsyncFunc := AsyncFunc<Integer, string>.
    Queue(
      AsyncFuncMapping.Action<Integer, Integer>(function(const Value: Integer): Extended
      begin
        //First Step from Integer (input type) to Extended
      end)).
    &Then(
      AsyncFuncMapping.Action<Extended, Boolean>(function(const Value: Extended): Boolean
      begin
        //Second Step from Extended to Boolean
      end)). 
    &Then(
      AsyncFuncMapping.Action<Boolean, string>(function(const Value: Boolean): string
      begin
        //Final Step from Boolean to string (result type)
      end)).
    Within(100, //Ms
      function: string
      begin
        //What to do and/or return if it expires.
        //If the function raises an exception then the status will still be expired, otherwise it will be finished
      end).
    Catch(
      function(const E: Exception): string
      begin
        //What to do and/or return in case of exception
      end);

  AsyncFunc.Run(100); //Fire and forget
  
  AsyncFunc.Run(100).Task; //Fire and get ITask
  
  FuncResult := AsyncProc.Run(100).Resolve; //Fire and wait
end;
```

## Signals and slots

Unit: `Fido.Slots.Intf`.

When you work with MVVM, it is easy to consume the View model from the View. The binding functionalities from [FidoGui](./FidoGui.md) allow you to easily bind components and entities (or other components). But, to be really workable, a solution should also provide a mechanism to link, without coupling, the View model to the View (or two components that are not supposed to see each other).

We based our functionalities on the signal and slots from the Qt C++ library.

##### The mechanism

The mechanism is pretty simple:

- Signals are messages broadcasted by an `IObserver` 
- Signals values must be formatted as `TArray<TValue>`
- Slots can be either a `Spring.TAction<TArray<TValue>>`, or a `public` procedure whose parameters are compatible with the signal. If the signal values are not in the same format, there is the possibility to map the values to the parameters required by the slot.  
- The signal can contain a number of parameters greater than the number required by the slot.

##### Usage

You can bind signals and slots in two ways:

The first is by code:

```pascal
type
  TMainView = class(TForm)
    ...
  private
    FSlots: ISlots;
    FViewModel: IMainViewModel;

    ...
    procedure SetupTokenSchedulerSlots;
    procedure SetupViewModelSlots;
  public
    ...
    procedure SetMemo(const Text: string);
    procedure OnLogStatusChanged(const Logged: Boolean);
  end;
  
implementation

...

procedure TMainView.SetupTokenSchedulerSlots;
begin
  // When the FTokenScheduler broadcasts the LOGGED_MESSAGE message the MainView.OnLogStatusChanged procedure will be called
  Slots.RegisterWithClass<TMainView>(
    FSlots,
    FTokenScheduler,
    LOGGED_MESSAGE,
    ftSynched,
    Self,
    'OnLogStatusChanged',
    function(params: TArray<TValue>): TArray<TValue>
    begin
      SetLength(Result, 1);
      Result[0] := Params[0].AsType<Integer> = 1; // Converts from Integer to Boolean; 
    end);
    
  // When the FTokenScheduler broadcasts the TOKEN_CHANGED_MESSAGE message the MainView.SetMemo procedure will be called
  Slots.RegisterWithClass<TMainView>(
    FSlots,
    FTokenScheduler,
    TOKEN_CHANGED_MESSAGE,
    ftSynched,
    Self,
    'SetMemo');

  // When the FTokenScheduler broadcasts the TOKEN_REFRESH_FAILED_MESSAGE message the anonymous action will be called.
  FSlots.Register(
    FTokenScheduler,
    TOKEN_REFRESH_FAILED_MESSAGE,
    ftSynched,
    procedure(const Params: TArray<TValue>)
    var
      Exc: Exception;
    begin
      Exc := Params[0].AsType<Exception>;
      if Exc is EFidoClientApiException then
        TDialogService.MessageDialog(
          Format('[%d] %s', [(Exc as EFidoClientApiException).ErrorCode, (Exc as EFidoClientApiException).ErrorMessage]),
          TMsgDlgType.mtError,
          [TMsgDlgBtn.mbOK],
          TMsgDlgBtn.mbOK,
          0,
          nil)
      else
        TDialogService.MessageDialog(
          Exc.Message,
          TMsgDlgType.mtError,
          [TMsgDlgBtn.mbOK],
          TMsgDlgBtn.mbOK,
          0,
          nil)
    end);
end;

procedure TMainView.SetupViewModelSlots;
begin
  // When theFViewModel broadcasts the VIEW_BUSY_MESSAGE message the anonymous action will be called.
  FSlots.Register(
    FViewModel,
    VIEW_BUSY_MESSAGE,
    ftSynched,
    procedure(const Params: TArray<TValue>)
    var
      Busy: Boolean;
    begin
      Busy := Params[0].AsType<Boolean>;

      MainToolBar.Enabled := not Busy;

      if Busy then
        Self.OnCloseQuery := FormCloseQueryNo
      else
        Self.OnCloseQuery := FormCloseQueryYes;
    end);
end;
```

The second one is by attributes, but in this case there is not parameters overriding functionality:

```pascal
type
  TMainView = class(TForm)
    ...
  private
    FTokenScheduler: ITokenScheduler;
    FSlots: ISlots;
    FViewModel: IMainViewModel;

    ....

    procedure SetupTokenSchedulerSlots;
    procedure SetupViewModelSlots;
  public
    ...

    [SignalToSlot('FTokenScheduler', TOKEN_CHANGED_MESSAGE, ftSynched)]
    procedure OnTokenChanged(const Text: string);
    [SignalToSlot('FTokenScheduler', LOGGED_MESSAGE, ftSynched)]
    procedure OnLogStatusChanged(const Logged: Boolean);
    [SignalToSlot('FTokenScheduler', TOKEN_REFRESH_FAILED_MESSAGE, ftSynched)]
    procedure OnTokenRefreshFailed(const E: Exception);
    [SignalToSlot('FViewModel', VIEW_BUSY_MESSAGE, ftSynched)]
    procedure OnBusyChange(const Busy: Boolean);

    ...
  end; 
  
implementation

...

procedure TMainView.SetupTokenSchedulerSlots;
begin
  Slots.Register(FSlots, FTokenScheduler, Self)
end;

procedure TMainView.SetupViewModelSlots;
begin
  Slots.Register(FSlots, FViewModel, Self)
end;
```
