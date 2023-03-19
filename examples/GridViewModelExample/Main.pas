unit Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Rtti,
  System.ImageList,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Edit,
  FMX.StdCtrls,
  FMX.MultiResBitmap,
  FMX.ImgList,

  Spring.Collections,

  Fido.Utilities,
  Fido.DesignPatterns.Observer.Intf,
  Fido.DesignPatterns.Observer.Notification.Intf,
  Fido.DesignPatterns.Observable.Intf,
  Fido.DesignPatterns.Observable.Delegated,
  Fido.Collections.DeepObservableList.Intf,
  Fido.Collections,

  Fido.Gui.Binding.Attributes,
  Fido.Gui.Fmx.Binding,
  Fido.Gui.Types,
  Fido.Gui.Fmx.Grid.ViewModel.Intf,

  Person,
  PeopleViewModel.Intf,
  PeopleViewModel;

type
  TMainView = class(TForm)
    [BindEvent('OnSortByHeaderClick', 'OnHeaderClick')]
    [BindEvent('OnSortByHeaderMove', 'OnColumnMoved')]
    [BindEvent('OnGetFieldValue', 'OnGetValue')]
    [BindEvent('OnSetFieldValue', 'OnSetValue')]
    Grid1: TGrid;
    ColId: TStringColumn;
    ColFirstName: TStringColumn;
    ColLastName: TStringColumn;
    Panel1: TPanel;
    [BidirectionalToObservableBinding('Filter', 'Text', 'OnTyping')]
    edtSearch: TEdit;
    [MethodToActionBinding('EditRecord', oeetBefore)]
    Button1: TButton;
    [MethodToActionBinding('AddRecord', oeetBefore)]
    Button2: TButton;
    [MethodToActionBinding('DeleteRecord', oeetBefore)]
    Button3: TButton;
    ImageList: TImageList;
    procedure Grid1DrawColumnHeader(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF);
  private
    FList: IDeepObservableList<IPerson>;
    FGridViewModel: IPeopleViewModel;
  public
    procedure AfterConstruction; override;
  end;

var
  MainView: TMainView;

implementation

{$R *.fmx}

{ TForm2 }

procedure TMainView.AfterConstruction;
begin
  inherited;
  FList := TCollections.GetListOfDeepObservable<IPerson>;
  FList.Add(TPerson.Create(TGuid.NewGuid, 'Mickey', 'Mouse'));
  FList.Add(TPerson.Create(TGuid.NewGuid, 'Minnie', 'Mouse'));
  FList.Add(TPerson.Create(TGuid.NewGuid, 'Donald', 'Duck'));
  FList.Add(TPerson.Create(TGuid.NewGuid, 'Daisy', 'Duck'));
  FList.Add(TPerson.Create(TGuid.NewGuid, 'Huey', 'Duck'));
  FList.Add(TPerson.Create(TGuid.NewGuid, 'Dewey', 'Duck'));
  FList.Add(TPerson.Create(TGuid.NewGuid, 'Louie', 'Duck'));
  FList.Add(TPerson.Create(TGuid.NewGuid, 'Goofy', 'Goof'));

  FGridViewModel := TPeopleViewModel.Create(Grid1, FList);
  Guibinding.Setup<IPeopleViewModel, TMainView>(FGridViewModel, Self);
  Guibinding.MethodsSetup<IPeopleViewModel, TMainView>(FGridViewModel, Self);
end;

procedure TMainView.Grid1DrawColumnHeader(Sender: TObject; const Canvas: TCanvas;
  const Column: TColumn; const Bounds: TRectF);
var
  Item: TCustomBitmapItem;
  Size: TSize;
begin
  case TGridColumnSortType(Column.Tag) of
    TGridColumnSortType.Asc: if not Grid1.Images.BitmapItemByName('Down', Item, Size) then
      Exit;
    TGridColumnSortType.Desc: if not Grid1.Images.BitmapItemByName('Up', Item, Size) then
      Exit;
    TGridColumnSortType.None: Exit;
  end;
  Canvas.DrawBitmap(Item.Bitmap, Item.Bitmap.BoundsF, TRectF.Create(Bounds.Width - Bounds.Height, 0, Bounds.Width, Bounds.Height), 1);
end;

end.
