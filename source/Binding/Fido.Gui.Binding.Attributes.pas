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

unit Fido.Gui.Binding.Attributes;

interface

uses
  System.SysUtils,

  Fido.Gui.Types;

type
  // Base attribute for Bindings
  GuiBindingAttribute = class abstract(TCustomAttribute);



  // Unidirectional binding from Observable to Gui component
  UnidirectionalToGuiBindingAttribute = class(GuiBindingAttribute)
  private
    FSourceAttributeName: string;
    FDestinationAttributeName: string;
  public
    constructor Create(const SourceAttributeName: string; const DestinationAttributeName: string);

    property SourceAttributeName: string read FSourceAttributeName;
    property DestinationAttributeName: string read FDestinationAttributeName;
  end;

  // Unidirectional binding from Gui component to Observable
  UnidirectionalToObservableBindingAttribute = class(GuiBindingAttribute)
  private
    FSourceAttributeName: string;
    FSourceEventName: string;
    FDestinationAttributeName: string;
  public
    constructor Create(const SourceAttributeName: string; const SourceEventName: string; const DestinationAttributeName: string);

    property SourceAttributeName: string read FSourceAttributeName;
    property SourceEventName: string read FSourceEventName;
    property DestinationAttributeName: string read FDestinationAttributeName;
  end;

  // Unidirectional binding from Observable to Gui component
  BidirectionalToObservableBindingAttribute = class(GuiBindingAttribute)
  private
    FSourceAttributeName: string;
    FDestinationAttributeName: string;
    FDestinationEventName: string;
  public
    constructor Create(const SourceAttributeName: string; const DestinationAttributeName: string; const DestinationEventName: string);

    property SourceAttributeName: string read FSourceAttributeName;
    property DestinationAttributeName: string read FDestinationAttributeName;
    property DestinationEventName: string read FDestinationEventName;
  end;

  MethodToActionBindingAttribute = class(GuiBindingAttribute)
  private
    FObservableMethodName: string;
    FOriginalEventExecutionType :TOriginalEventExecutionType;
  public
    constructor Create(const ObservableMethodName: string; const OriginalEventExecutionType :TOriginalEventExecutionType = oeetBefore);

    property ObservableMethodName: string read FObservableMethodName write FObservableMethodName;
    property OriginalEventExecutionType :TOriginalEventExecutionType read FOriginalEventExecutionType write FOriginalEventExecutionType;
  end;

  BindEventAttribute = class(GuiBindingAttribute)
  private
    FObservableEventFunc: string;
    FComponentEvent: string;
  public
    constructor Create(const ObservableEventFunc: string; const ComponentEvent: string);

    property ObservableEventFunc: string read FObservableEventFunc;
    property ComponentEvent: string read FComponentEvent;
  end;

implementation

{ BidirectionalToObservableBindingAttribute }

constructor BidirectionalToObservableBindingAttribute.Create(
  const SourceAttributeName,
        DestinationAttributeName,
        DestinationEventName: string);
begin
  inherited Create;
  FSourceAttributeName := SourceAttributeName;
  FDestinationAttributeName := DestinationAttributeName;
  FDestinationEventName := DestinationEventName;
end;

{ UnidirectionalToVclBindingAttribute }

constructor UnidirectionalToGuiBindingAttribute.Create(
  const SourceAttributeName,
        DestinationAttributeName: string);
begin
  inherited Create;
  FSourceAttributeName := SourceAttributeName;
  FDestinationAttributeName := DestinationAttributeName;
end;

{ UnidirectionalToObservableBindingAttribute }

constructor UnidirectionalToObservableBindingAttribute.Create(
  const SourceAttributeName,
        SourceEventName,
        DestinationAttributeName: string);
begin
  inherited Create;
  FSourceAttributeName := SourceAttributeName;
  FSourceEventName := SourceEventName;
  FDestinationAttributeName := DestinationAttributeName;
end;

{ MethodToActionBindingAttribute }

constructor MethodToActionBindingAttribute.Create(
  const ObservableMethodName: string;
  const OriginalEventExecutionType: TOriginalEventExecutionType);
begin
  inherited Create;
  FObservableMethodName := ObservableMethodName;
  FOriginalEventExecutionType := OriginalEventExecutionType
end;

{ BindEventAttribute }

constructor BindEventAttribute.Create(
  const ObservableEventFunc: string;
  const ComponentEvent: string);
begin
  FObservableEventFunc := ObservableEventFunc;
  FComponentEvent := ComponentEvent;
end;

end.
