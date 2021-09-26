# Fido Gui documentation

Fido Gui contains features related to the binding of view models to VCL and Firemonkey components.

I love the [MVVM](https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93viewmodel) architectural pattern and I am not a great fan of the live bindings technology that's shipped with Delphi.

For this reason I wrote my own binding library, feel free to use it, if you want.

The `MVVMExample` project shows how binding can be achieved, the main points are:

- Make use of  the `TDelegateObservable` as base for the entity that needs binding. 

- Make use of  the `TDelegateObservable` as base for the view model that will consume the Entity.

- Decorate the forms components that need binding with the `UnidirectionalToGuiBinding`, `UnidirectionalToObservableBinding` or `BidirectionalToObservableBinding` attributes you can find in `Fido.Gui.Binding.Attributes`.

- Decorate the forms actions that need binding with the `MethodToActionBinding` attribute you can find in `Fido.Gui.Binding.Attributes`.

- Bind view model and Form by calling:

  ```pascal
    Guibinding.Setup<ISongViewModel, TSongView>(FSongViewModel, Self);
    Guibinding.MethodsSetup<ISongViewModel, TSongView>(FSongViewModel, Self);
  ```

