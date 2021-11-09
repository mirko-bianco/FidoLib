(*
 * Copyright 2021 Mirko Bianco (email: writetomirko@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without Apiriction, including without limitation the rights
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

unit Fido.Api.Server.Resource.Attributes;

interface

uses
  System.Rtti,

  Fido.Http.Types;

type
  WebAttribute = class abstract (TCustomAttribute);

  BaseUrlAttribute = class(WebAttribute)
  private
    FBaseUrl: string;
  public
    constructor Create(const BaseUrl: string);

    property BaseUrl: string read FBaseUrl;
  end;

  PathAttribute = class(WebAttribute)
  private
    FMethod: THttpMethod;
    FPath: string;
  public
    constructor Create(const Method: THttpMethod; const Path: string);

    property Method: THttpMethod read FMethod;
    property Path: string read FPath;
  end;

  MimeTypeAttribute = class abstract (WebAttribute)
  private
    FMimeType: TMimeType;
  public
    constructor Create(const MimeType: TMimeType);

    property MimeType: TMimeType read FMimeType;
  end;

  ConsumesAttribute = class(MimeTypeAttribute);
  ProducesAttribute = class(MimeTypeAttribute);

  ResponseCodeAttribute = class(WebAttribute)
  private
    FResponseCode: Integer;
    FResponseText: string;
  public
    constructor Create(const ResponseCode: Integer; const ResponseText: string);

    property ResponseCode: Integer read FResponseCode;
    property ResponseText: string read FResponseText;
  end;

  MiddlewareAttribute = class abstract (WebAttribute)
  private
    FStepName: string;
    FCommaSeparatedParams: string;
  public
    constructor Create(const StepName: string; const CommaSeparatedParams: string = '');

    property StepName: string read FStepName;
    property CommaSeparatedParams: string read FCommaSeparatedParams;
  end;

  ParamAttribute = class abstract(WebAttribute)
  private
    FParamName: string;
  public
    constructor Create(const ParamName: string = '');

    property ParamName: string read FParamName;
  end;

  PathParamAttribute = class(ParamAttribute);
  FormParamAttribute = class(ParamAttribute);
  BodyParamAttribute = class(ParamAttribute);
  HeaderParamAttribute = class(ParamAttribute);
  QueryParamAttribute = class(ParamAttribute);

  WebSocketPathAttribute = class(WebAttribute)
  private
    FPath: string;
  public
    constructor Create(const Path: string);

    property Path: string read FPath;
  end;

  RequestMiddlewareAttribute = MiddlewareAttribute;
  ResponseMiddlewareAttribute = MiddlewareAttribute;

implementation

{ PathAttribute }

constructor PathAttribute.Create(
  const Method: THttpMethod;
  const Path: string);
begin
  FMethod := Method;
  FPath := Path;
end;

{ ParamAttribute }

constructor ParamAttribute.Create(const ParamName: string);
begin
  FParamName := ParamName
end;

{ BaseUrlAttribute }

constructor BaseUrlAttribute.Create(const BaseUrl: string);
begin
  FBaseUrl := BaseUrl;
end;

{ ConsumesAttribute }

constructor MimeTypeAttribute.Create(const MimeType: TMimeType);
begin
  FMimeType := MimeType;
end;

{ ResponseCodeAttribute }

constructor ResponseCodeAttribute.Create(
  const ResponseCode: Integer;
  const ResponseText: string);
begin
  FResponseCode := ResponseCode;
  FResponseText := ResponseText;
end;

{ PipelineStepAttribute }

constructor MiddlewareAttribute.Create(const StepName: string; const CommaSeparatedParams: string);
begin
  FStepName := StepName;
  FCommaSeparatedParams := CommaSeparatedParams;
end;

{ WebSocketPathAttribute }

constructor WebSocketPathAttribute.Create(const Path: string);
begin
  FPath := Path;
end;

end.
