

# Fido Library documentation

The Fido library has been created to make the life of a Delphi developer easier by following the "**describe behaviour instead of coding it, whenever is possible**" design principle.

The Fido library is highly opinionated but it won't force you to adopt it entirely, you can just use the virtual database, or the virtual client API or the JSON marshalling/unmarshalling, if you feel like.

The Fido library is mostly based on composition and attributes, hence it won't force you to revolutionize your current base hierarchy structure.

## Acknowledgements

Fido library was born, with a different name and different scopes, several years ago. In one of its many incarnations it has been used and improved while I was employed at one of the biggest online shops of the Netherlands, and it's during that time that it bloomed to most of its functionalities.

And it was working with amazing people that gave me the inspiration. You will notice that the Fido library makes extensive usage of the `TVirtualInterface` class, and for this I want to thank my great friend and amazing developer Michal Kulczycki. He is the guy who came up with the idea of using "Virtual things" to describe behaviour instead of coding it, and wrote the first implementation of the virtual statement. I just tagged along and added bits and pieces. 

He is also the guy behind the observer/(delegate)observable implementation.

## Contributing

Help is always welcome and appreciated, and I will embrace any good idea that comes this way.

But remember that this library was born because I couldn't find anything around that was working the way I wanted, so I won't accept PRs that try to change the nature of the library, that is:

- Describe behaviour instead of coding it, whenever is possible.
- Use composition as much as possible and avoid class hierarchies. Anyone should be able to start with the library without the need to change and rewrite their own codebase.
- Think (as a library designer) about your user (the developer). Clear and elegant interfaces and, if possible, one single way to achieve a result.

If you are willing to play by the rules then let's have fun togheter. 

## Dependencies

The Fido library depends on the following open source libraries:

[Spring4D](https://bitbucket.org/sglienke/spring4d/src/master/)

[Delphi JOSE and JWT Library](https://github.com/paolo-rossi/delphi-jose-jwt)

[DUnitX](https://github.com/VSoftTechnologies/DUnitX)

## Installation

1) clone the source to a location of your choice
2) Create a system environment variable `FIDOLIB` pointing to the `fidolib\source` folder
3) Go to Delphi menu -> Tools -> Options... -> Delphi Options  -> Library
4) add `$(FIDOLIB);$(FIDOLIB)\Actions;$(FIDOLIB)\Adapter;$(FIDOLIB)\Binding;$(FIDOLIB)\Collections;$(FIDOLIB)\Db;$(FIDOLIB)\DesignPatterns;$(FIDOLIB)\Environment;$(FIDOLIB)\Events;$(FIDOLIB)\Http;$(FIDOLIB)\Interfaces;$(FIDOLIB)\Json;$(FIDOLIB)\JWT;$(FIDOLIB)\Logging;$(FIDOLIB)\Model;$(FIDOLIB)\Resources;$(FIDOLIB)\Api\Client;$(FIDOLIB)\Api\Client\ElasticSearch;$(FIDOLIB)\Api\Client\ElasticSearch\Dto;$(FIDOLIB)\Api\Client\VirtualApi;$(FIDOLIB)\Api\Server;$(FIDOLIB)\Api\Server\Resources;$(FIDOLIB)\Testing;$(FIDOLIB)\VirtualStatement;$(FIDOLIB)\Web\Server;$(FIDOLIB)\Web\Server\WebSocket;$(FIDOLIB)\Async` to the Library path (for all the available platforms.

## Building

1) Open the `FidoLibGroup.groupproj`
2) Build all the projects

## The packages

The Fido Library is made of 3 main packages

- **[FidoCore](./FidoCore.md)** - Contains the core functionalities that are not linked or dependent on any specific OS.

- **[FidoWin](./FidoWin.md)** - Contains the functionalities linked to Windows, such as the ADO implementation of the DB interfaces.

- **[FidoGui/FidoVcl/FidoFmx](./FidoGui.md)** - Contains the functionalities related to the UI, such as the binding of view models to VCL and FireMonkey components.

  

