# Fido Gui documentation

Fido Gui contains features related to the binding of view models to VCL (FidoVcl) and Firemonkey (FidoFmx) components.

I love the [MVVM](https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93viewmodel) architectural pattern and I am not a great fan of the live bindings technology that's shipped with Delphi.

For this reason I wrote my own binding library, feel free to use it, if you want.

The `MVVMExample` project shows how binding can be achieved, the main points are:

- Make use of  the `TDelegateObservable` as base for the entity that needs binding. 

- Make use of  the `TDelegateObservable` as base for the view model that will consume the Entity.

- Decorate the forms components that need binding with the `UnidirectionalToSyncGuiBinding` (when the component needs synchronization), `UnidirectionalToNoSyncGuiBinding` (when the component doesn't need synchronization), `UnidirectionalToObservableBinding` or `BidirectionalToObservableBinding` attributes you can find in `Fido.Gui.Binding.Attributes`.

- Decorate the forms actions that need binding with the `MethodToActionBinding` attribute you can find in `Fido.Gui.Binding.Attributes`.

- Bind view model and Form by calling:

  ```pascal
    Guibinding.Setup<ISongViewModel, TSongView>(FSongViewModel, Self);
    Guibinding.MethodsSetup<ISongViewModel, TSongView>(FSongViewModel, Self);
  ```

The `GridViewModelExample` project shows how binding of a grid and a list can be achieved, the main points are:

- Make use of the `TDelegateObservable` as base for the entity that needs binding. 
- Make use of the `TAbstractReadonlyGridView<T>`or `TAbstractWriteableGridView<T>` as base for the grid view model that will consume the List.
- Make use of the `IFilteredGridViewModelHelper<T, FilterPayload>` helper if you need to filter the list through the view model.
- Make use of the `ISortedByColumnGridViewModelHelper<T>` helper if you need to sort the list through the grid columns position and header click.
- Decorate the grid with the `BindEvent` attribute you can find in `Fido.Gui.Binding.Attributes`, for the events `OnGetValue`, `OnSetValue`, `OnColumnMoved` or `OnHeaderClick`.
