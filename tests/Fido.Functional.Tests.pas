unit Fido.Functional.Tests;

interface

uses
  System.SysUtils,
  System.DateUtils,
  System.Threading,

  DUnitX.TestFramework,

  Spring,

  Fido.Exceptions,
  Fido.DesignPatterns.Retries,
  Fido.Functional,
  Fido.Functional.Ifs,
  Fido.Functional.Retries,
  Fido.Functional.Tries;

type
  IGateway = interface(IInvokable)
    ['{10B6C826-BB8A-4B59-BCEF-9C91AFADE1CB}']
    function Run(const Index: Integer): Context<Integer>;
  end;

  TGateway = class(TInterfacedObject, IGateway)
  private
    function DoRun(const Index: Integer): Integer;
  public
    function Run(const Index: Integer): Context<Integer>;
  end;

  IRepo = interface(IInvokable)
    ['{679F48A0-6702-4A04-8B67-2F3A40196B4A}']
    function Run(const Index: Integer): Context<Integer>;
  end;

  TRepo = class(TInterfacedObject, IRepo)
  private
    FGateway: IGateway;

    function DoRun(const Index: Integer): Context<Integer>;
    function DoRunGateway(const Index: Integer): Context<Integer>;
  public
    constructor Create(const Gateway: IGateway);

    function Run(const Index: Integer): Context<Integer>;
  end;

  EFunctionalTests = class(EFidoException);
  EFunctionalTestsUncaught = class(EFidoException);

  [TestFixture]
  TFunctionalTests = class
  public
    [Test]
    Procedure TestSeveralClasses;

    [Test]
    procedure UnassignedContext;

    [Test]
    procedure IsAsyncIsTrueWhenContextIsFuture;

    [Test]
    procedure IsAsyncIsFalseWhenContextIsNotFuture;

    [Test]
    procedure ImplicitToType;

    [Test]
    procedure ImplicitToFunc;

    [Test]
    procedure ImplicitFromType;

    [Test]
    procedure ImplicitFromFunc;

    [Test]
    procedure MapFunctor;

    [Test]
    procedure MapApplicative;

    [Test]
    procedure MapMonad;

    [Test]
    procedure MapAsyncFunctor;

    [Test]
    procedure MapAsyncApplicative;

    [Test]
    procedure MapAsyncMonad;

    [Test]
    procedure VoidMapProcFunctorFuncToFunctorProc;

    [Test]
    procedure VoidMapProcFunctorProcToFunctorFunc;

    [Test]
    procedure VoidMapProcProcToFunctorFunc;

    [Test]
    procedure VoidMapFuncFunctorFuncToFunc;

    [Test]
    procedure VoidMapFuncFuncToFunctorFunc;

    [Test]
    procedure IfThenFunctorWhenTrue;

    [Test]
    procedure IfThenFunctorWhenFalse;

    [Test]
    procedure IfThenFunctorWhenTrue2;

    [Test]
    procedure IfThenFunctorWhenFalse2;

    [Test]
    procedure IfThenFunctorWhenTrue3;

    [Test]
    procedure IfThenFunctorWhenFalse3;

    [Test]
    procedure IfThenFunctorWhenTrue4;

    [Test]
    procedure IfThenFunctorWhenFalse4;

    [Test]
    procedure IfThenMonadWhenTrue;

    [Test]
    procedure IfThenMonadWhenFalse;

    [Test]
    procedure IfThenMonadWhenTrue2;

    [Test]
    procedure IfThenMonadWhenFalse2;

    [Test]
    procedure IfThenMonadWhenTrue3;

    [Test]
    procedure IfThenMonadWhenFalse3;

    [Test]
    procedure IfThenMonadWhenTrue4;

    [Test]
    procedure IfThenMonadWhenFalse4;

    [Test]
    procedure ThenElseWhenTrue;

    [Test]
    procedure ThenElseWhenFalse;

    [Test]
    procedure ThenElseWhenTrue2;

    [Test]
    procedure ThenElseWhenFalse2;

    [Test]
    procedure RetryFunctorWorks;

    [Test]
    procedure RetryFunctorAsyncWorks;

    [Test]
    procedure RetryOfFunctorWorks;

    [Test]
    procedure RetryOfFunctorAsyncWorks;

    [Test]
    procedure RetryOfMonadWorks;

    [Test]
    procedure RetryOfMonadAsyncWorks;

    [Test]
    procedure TryFunctorDoesNotRaiseAnyExceptionWhenItWorks;

    [Test]
    procedure TryFunctorRaisesAnExceptionWhenItDoesNotWork;

    [Test]
    procedure TryMonadDoesNotRaiseAnyExceptionWhenItWorks;

    [Test]
    procedure TryMonadRaisesAnExceptionWhenItDoesNotWork;

    [Test]
    procedure TryAsyncFunctorDoesNotRaiseAnyExceptionWhenItWorks;

    [Test]
    procedure TryAsyncFunctorRaisesAnExceptionWhenItDoesNotWork;

    [Test]
    procedure TryAsyncMonadDoesNotRaiseAnyExceptionWhenItWorks;

    [Test]
    procedure TryAsyncMonadRaisesAnExceptionWhenItDoesNotWork;

    [Test]
    procedure TryMatchItWorks;
  end;

implementation

procedure TFunctionalTests.ImplicitFromFunc;
var
  Ctx: Context<Integer>;
begin
  Ctx := function: Integer
    begin
      Result := 100;
    end;

  Assert.AreEqual(True, Ctx.IsAssigned);
  Assert.AreEqual<Integer>(100, Ctx.Value);
end;

procedure TFunctionalTests.ImplicitFromType;
var
  Ctx: Context<Integer>;
begin
  Ctx := 100;

  Assert.AreEqual(True, Ctx.IsAssigned);
  Assert.AreEqual<Integer>(100, Ctx.Value);
end;

procedure TFunctionalTests.ImplicitToFunc;
var
  Ctx: Context<Integer>;
  Func: Func<Integer>;
begin
  Ctx := Context<Integer>.New(100);

  Func := Ctx;

  Assert.AreEqual<Integer>(100, Func());
end;

procedure TFunctionalTests.ImplicitToType;
var
  Ctx: Context<Integer>;
begin
  Ctx := Context<Integer>.New(100);

  Assert.AreEqual<Integer>(100, Ctx.Value);
end;

procedure TFunctionalTests.IsAsyncIsFalseWhenContextIsNotFuture;
var
  Ctx: Context<Integer>;
begin
  Ctx := Context<Integer>.New(100);

  Assert.AreEqual(False, Ctx.IsAsync);
end;

procedure TFunctionalTests.IsAsyncIsTrueWhenContextIsFuture;
var
  Ctx: Context<Integer>;
begin
  Ctx := Context<Integer>.New(function: Integer
    begin
      Result := 100;
    end, 1000);

  Assert.AreEqual(True, Ctx.IsAsync);
end;

procedure TFunctionalTests.MapApplicative;
var
  Value: string;
  Appl: Context<Context<Integer>.FunctorFunc<string>>;
begin
  Appl := Context<Context<Integer>.FunctorFunc<string>>.New(function(const Input: Integer): string
    begin
      Result := IntToStr(Input);
    end);

  Value := Context<Integer>.New(100).Map<string>(Appl);

  Assert.AreEqual('100', Value);
end;

procedure TFunctionalTests.MapAsyncApplicative;
var
  Value: string;
  Appl: Context<Context<Integer>.FunctorFunc<string>>;
begin
  Appl := Context<Context<Integer>.FunctorFunc<string>>.New(function(const Input: Integer): string
    begin
      Result := IntToStr(Input);
    end);

  Value := Context<Integer>.New(100).MapAsync<string>(Appl, 100);

  Assert.AreEqual('100', Value);
end;

procedure TFunctionalTests.MapAsyncFunctor;
var
  Value: string;
begin
  Value := Context<Integer>.New(100).MapAsync<string>(function(const Input: Integer): string
    begin
      Result := IntToStr(Input);
    end,
    100);

  Assert.AreEqual('100', Value);
end;

procedure TFunctionalTests.MapAsyncMonad;
var
  Value: string;
begin
  Value := Context<Integer>.New(100).MapAsync<string>(function(const Input: Integer): Context<string>
    begin
      Result := Context<string>.New(IntToStr(Input));
    end,
    100);

  Assert.AreEqual('100', Value);
end;

procedure TFunctionalTests.MapFunctor;
var
  Value: string;
begin
  Value := Context<Integer>.New(100).Map<string>(function(const Input: Integer): string
    begin
      Result := IntToStr(Input);
    end);

  Assert.AreEqual('100', Value);
end;

procedure TFunctionalTests.MapMonad;
var
  Value: string;
begin
  Value := Context<Integer>.New(100).Map<string>(function(const Input: Integer): Context<string>
    begin
      Result := Context<string>.New(IntToStr(Input));
    end);

  Assert.AreEqual('100', Value);
end;

procedure TFunctionalTests.TestSeveralClasses;
var
  Gateway: IGateway;
  Repo: IRepo;
  Result: Integer;
begin
  Gateway := TGateway.Create;
  Repo := TRepo.Create(Gateway);

  Result := Repo.Run(2);

  Assert.AreEqual(8, Result);

  Repo := nil;
  Gateway := nil;
end;

procedure TFunctionalTests.ThenElseWhenFalse;
var
  Result: Context<Integer>;
  WhenTrue: Context<Integer>;
  WhenFalse: Context<Integer>;
begin
  WhenTrue := function: Integer
    begin
      Result := 1;
    end;
  WhenFalse := function: Integer
    begin
      Result := 0;
    end;

  Result := ThenElse.New(False).&Then<Integer>(WhenTrue, WhenFalse);

  Assert.AreEqual(0, Result.Value);
end;

procedure TFunctionalTests.ThenElseWhenFalse2;
var
  Result: Context<Integer>;
  WhenTrue: Context<Integer>;
begin
  WhenTrue := function: Integer
    begin
      Result := 1;
    end;

  Result := ThenElse.New(False).&Then<Integer>(WhenTrue);

  Assert.AreEqual(0, Result.Value);
end;

procedure TFunctionalTests.ThenElseWhenTrue;
var
  Result: Context<Integer>;
  WhenTrue: Context<Integer>;
  WhenFalse: Context<Integer>;
begin
  WhenTrue := function: Integer
    begin
      Result := 1;
    end;
  WhenFalse := function: Integer
    begin
      Result := 0;
    end;

  Result := ThenElse.New(True).&Then<Integer>(WhenTrue, WhenFalse);

  Assert.AreEqual(1, Result.Value);
end;

procedure TFunctionalTests.ThenElseWhenTrue2;
var
  Result: Context<Integer>;
  WhenTrue: Context<Integer>;
begin
  WhenTrue := function: Integer
    begin
      Result := 1;
    end;

  Result := ThenElse.New(True).&Then<Integer>(WhenTrue);

  Assert.AreEqual(1, Result.Value);
end;

procedure TFunctionalTests.TryAsyncFunctorDoesNotRaiseAnyExceptionWhenItWorks;
var
  Result: Integer;
  Flag: Boolean;
begin
  Flag := False;
  Assert.WillNotRaiseAny(procedure
    begin
      Result := &Try<string>.New('100').MapAsync<Integer>(StrToInt, 100).Match(EFunctionalTests, '', procedure
        begin
          Flag := True;
        end);
    end);

  Assert.AreEqual(100, Result);
  Assert.AreEqual(True, Flag);
end;

procedure TFunctionalTests.TryAsyncFunctorRaisesAnExceptionWhenItDoesNotWork;
var
  Result: Integer;
  Flag: Boolean;
begin
  Flag := False;
  Assert.WillRaise(procedure
    begin
      Result := &Try<string>.New('100sss').MapAsync<Integer>(StrToInt, 100).Match(EFunctionalTests, '', procedure
        begin
          Flag := True;
        end);
    end,
    EFunctionalTests);

  Assert.AreEqual(True, Flag);
end;

procedure TFunctionalTests.TryAsyncMonadDoesNotRaiseAnyExceptionWhenItWorks;
var
  Result: Integer;
  Flag: Boolean;
begin
  Flag := False;
  Assert.WillNotRaiseAny(procedure
    begin
      Result := &Try<string>.New('100').MapAsync<Integer>(function(const Value: string): Context<Integer>
        begin
          Result := StrToInt(Value);
        end,
        100).Match(EFunctionalTests, '', procedure
        begin
          Flag := True;
        end);
    end);

  Assert.AreEqual(100, Result);
  Assert.AreEqual(True, Flag);
end;

procedure TFunctionalTests.TryAsyncMonadRaisesAnExceptionWhenItDoesNotWork;
var
  Result: Integer;
  Flag: Boolean;
begin
  Flag := False;
  Assert.WillRaise(procedure
    begin
      Result := &Try<string>.New('100sss').MapAsync<Integer>(function(const Value: string): Context<Integer>
        begin
          Result := StrToInt(Value);
        end,
        100).Match(EFunctionalTests, '', procedure
        begin
          Flag := True;
        end);
    end,
    EFunctionalTests);

  Assert.AreEqual(True, Flag);
end;

procedure TFunctionalTests.TryFunctorDoesNotRaiseAnyExceptionWhenItWorks;
var
  Result: Integer;
  Flag: Boolean;
begin
  Flag := False;
  Assert.WillNotRaiseAny(procedure
    begin
      Result := &Try<string>.New('100').Map<Integer>(StrToInt).Match(EFunctionalTests, '', procedure
        begin
          Flag := True;
        end);
    end);

  Assert.AreEqual(100, Result);
  Assert.AreEqual(True, Flag);
end;

procedure TFunctionalTests.TryFunctorRaisesAnExceptionWhenItDoesNotWork;
var
  Result: Integer;
  Flag: Boolean;
begin
  Flag := False;
  Assert.WillRaise(procedure
    begin
      Result := &Try<string>.New('100sss').Map<Integer>(StrToInt).Match(EFunctionalTests, '', procedure
        begin
          Flag := True;
        end);
    end,
    EFunctionalTests);

  Assert.AreEqual(True, Flag);
end;

procedure TFunctionalTests.TryMatchItWorks;
var
  Result: Boolean;
begin
  Assert.WillNotRaiseAny(procedure
    begin
      Result := &Try<Integer>.New(function: Integer
        begin
          Result := StrToInt('100');
        end).Match;
    end);

  Assert.AreEqual(True, Result);
end;

procedure TFunctionalTests.TryMonadDoesNotRaiseAnyExceptionWhenItWorks;
var
  Result: Integer;
  Flag: Boolean;
begin
  Flag := False;
  Assert.WillNotRaiseAny(procedure
    begin
      Result := &Try<string>.New('100').Map<Integer>(function(const Value: string): Context<Integer>
        begin
          Result := StrToInt(Value);
        end).Match(EFunctionalTests, '', procedure
        begin
          Flag := True;
        end);
    end);

  Assert.AreEqual(100, Result);
  Assert.AreEqual(True, Flag);
end;

procedure TFunctionalTests.TryMonadRaisesAnExceptionWhenItDoesNotWork;
var
  Result: Integer;
  Flag: Boolean;
begin
  Flag := False;
  Assert.WillRaise(procedure
    begin
      Result := &Try<string>.New('100sss').Map<Integer>(function(const Value: string): Context<Integer>
        begin
          Result := StrToInt(Value);
        end).Match(EFunctionalTests, '', procedure
        begin
          Flag := True;
        end);
    end,
    EFunctionalTests);

  Assert.AreEqual(True, Flag);
end;

procedure TFunctionalTests.UnassignedContext;
var
  Ctx: Context<Boolean>;
begin
  Assert.AreEqual(False, Ctx.IsAssigned);
end;

procedure TFunctionalTests.VoidMapProcFunctorProcToFunctorFunc;
var
  FunctorFunc: Context<string>.FunctorFunc<Void>;
  Count: Integer;
  AVoid: Void;
begin
  Count := 0;
  FunctorFunc := Void.MapProc<string>(procedure(const Value: string)
    begin
      Inc(Count);
    end);

  AVoid := FunctorFunc('A');

  Assert.AreEqual(1, Count);
end;

procedure TFunctionalTests.VoidMapProcProcToFunctorFunc;
var
  FunctorFunc: Context<Void>.FunctorFunc<Void>;
  Count: Integer;
begin
  Count := 0;

  FunctorFunc := Void.MapProc(procedure
    begin
      Inc(Count);
    end);

  FunctorFunc(Void.Get);

  Assert.AreEqual<Integer>(1, Count);
end;

procedure TFunctionalTests.RetryOfFunctorWorks;
var
  Result: string;
begin
  Result := Retry<Integer>.New(100).Map<string>(function(const Value: Integer): string
    begin
      Result := IntToStr(Value)
    end,
    Retries.GetRetriesOnExceptionFunc());

  Assert.AreEqual('100', Result);
end;

procedure TFunctionalTests.RetryOfMonadAsyncWorks;
var
  Result: string;
begin
  Result := Retry<Integer>.New(100).MapAsync<string>(function(const Value: Integer): Context<string>
    begin
      Result := IntToStr(Value)
    end,
    100,
    False,
    Retries.GetRetriesOnExceptionFunc());

  Assert.AreEqual('100', Result);
end;

procedure TFunctionalTests.RetryOfMonadWorks;
var
  Result: string;
begin
  Result := Retry<Integer>.New(100).Map<string>(function(const Value: Integer): Context<string>
    begin
      Result := IntToStr(Value)
    end,
    Retries.GetRetriesOnExceptionFunc());

  Assert.AreEqual('100', Result);
end;

procedure TFunctionalTests.RetryOfFunctorAsyncWorks;
var
  Result: string;
begin
  Result := Retry<Integer>.New(100).MapAsync<string>(function(const Value: Integer): string
    begin
      Result := IntToStr(Value)
    end,
    100,
    False,
    Retries.GetRetriesOnExceptionFunc());

  Assert.AreEqual('100', Result);
end;

procedure TFunctionalTests.RetryFunctorAsyncWorks;
var
  Result: Boolean;
begin
  Result := Retry.MapAsync<Boolean>(function: Boolean
    begin
      Result := True;
      Sleep(10);
    end,
    100,
    False,
    Retries.GetRetriesOnExceptionFunc());

  Assert.AreEqual(True, Result);
end;

procedure TFunctionalTests.RetryFunctorWorks;
var
  Result: Boolean;
begin
  Result := Retry.Map<Boolean>(function: Boolean
    begin
      Result := True;
    end,
    Retries.GetRetriesOnExceptionFunc());

  Assert.AreEqual(True, Result);
end;

procedure TFunctionalTests.IfThenFunctorWhenFalse;
var
  WhenTrue: Context<Integer>.FunctorFunc<Integer>;
  Result: Integer;
  Called: Boolean;
begin
  Called := False;
  WhenTrue := function(const Value: Integer): Integer
    begin
      Result := 1;
      Called := True;
    end;

  Result := &If<Integer>.New(1).Map(function(const Value: Integer): Boolean
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue, 0);

  Assert.AreEqual(0, Result);
  Assert.AreEqual(False, Called);
end;

procedure TFunctionalTests.IfThenFunctorWhenFalse2;
var
  WhenTrue: Context<Integer>.FunctorFunc<Integer>;
  Result: Integer;
  Called: Boolean;
begin
  Called := False;
  WhenTrue := function(const Value: Integer): Integer
    begin
      Result := 1;
      Called := True;
    end;

  Result := &If<Integer>.New(1).Map(function(const Value: Integer): Boolean
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue);

  Assert.AreEqual(0, Result);
  Assert.AreEqual(False, Called);
end;

procedure TFunctionalTests.IfThenFunctorWhenFalse3;
var
  WhenTrue: Context<Integer>;
  Result: Integer;
  Called: Boolean;
begin
  Called := False;
  WhenTrue := function: Integer
    begin
      Result := 1;
      Called := True;
    end;

  Result := &If<Integer>.New(1).Map(function(const Value: Integer): Boolean
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue, 0);

  Assert.AreEqual(0, Result);
  Assert.AreEqual(False, Called);
end;

procedure TFunctionalTests.IfThenFunctorWhenFalse4;
var
  WhenTrue: Context<Integer>;
  Result: Integer;
  Called: Boolean;
begin
  Called := False;
  WhenTrue := function: Integer
    begin
      Result := 1;
      Called := True;
    end;

  Result := &If<Integer>.New(1).Map(function(const Value: Integer): Boolean
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue);

  Assert.AreEqual(0, Result);
  Assert.AreEqual(False, Called);
end;

procedure TFunctionalTests.IfThenFunctorWhenTrue;
var
  WhenTrue: Context<Integer>.FunctorFunc<Integer>;
  Result: Integer;
begin
  WhenTrue := function(const Value: Integer): Integer
    begin
      Result := 1;
    end;

  Result := &If<Integer>.New(100).Map(function(const Value: Integer): Boolean
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue, 0);

  Assert.AreEqual(1, Result);
end;

procedure TFunctionalTests.IfThenFunctorWhenTrue2;
var
  WhenTrue: Context<Integer>.FunctorFunc<Integer>;
  Result: Integer;
begin
  WhenTrue := function(const Value: Integer): Integer
    begin
      Result := 1;
    end;

  Result := &If<Integer>.New(100).Map(function(const Value: Integer): Boolean
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue);

  Assert.AreEqual(1, Result);
end;

procedure TFunctionalTests.IfThenFunctorWhenTrue3;
var
  WhenTrue: Context<Integer>;
  Result: Integer;
begin
  WhenTrue := function: Integer
    begin
      Result := 1;
    end;

  Result := &If<Integer>.New(100).Map(function(const Value: Integer): Boolean
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue, 0);

  Assert.AreEqual(1, Result);
end;

procedure TFunctionalTests.IfThenFunctorWhenTrue4;
var
  WhenTrue: Context<Integer>;
  Result: Integer;
begin
  WhenTrue := function: Integer
    begin
      Result := 1;
    end;

  Result := &If<Integer>.New(100).Map(function(const Value: Integer): Boolean
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue);

  Assert.AreEqual(1, Result);
end;

procedure TFunctionalTests.IfThenMonadWhenFalse;
var
  WhenTrue: Context<Integer>.FunctorFunc<Integer>;
  Result: Integer;
  Called: Boolean;
begin
  Called := False;
  WhenTrue := function(const Value: Integer): Integer
    begin
      Result := 1;
      Called := True;
    end;

  Result := &If<Integer>.New(1).Map(function(const Value: Integer): Context<Boolean>
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue, 0);

  Assert.AreEqual(0, Result);
  Assert.AreEqual(False, Called);
end;

procedure TFunctionalTests.IfThenMonadWhenFalse2;
var
  WhenTrue: Context<Integer>.FunctorFunc<Integer>;
  Result: Integer;
  Called: Boolean;
begin
  Called := False;
  WhenTrue := function(const Value: Integer): Integer
    begin
      Result := 1;
      Called := True;
    end;

  Result := &If<Integer>.New(1).Map(function(const Value: Integer): Context<Boolean>
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue);

  Assert.AreEqual(0, Result);
  Assert.AreEqual(False, Called);
end;

procedure TFunctionalTests.IfThenMonadWhenFalse3;
var
  WhenTrue: Context<Integer>;
  Result: Integer;
  Called: Boolean;
begin
  Called := False;
  WhenTrue := function: Integer
    begin
      Result := 1;
      Called := True;
    end;

  Result := &If<Integer>.New(1).Map(function(const Value: Integer): Context<Boolean>
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue, 0);

  Assert.AreEqual(0, Result);
  Assert.AreEqual(False, Called);
end;

procedure TFunctionalTests.IfThenMonadWhenFalse4;
var
  WhenTrue: Context<Integer>;
  Result: Integer;
  Called: Boolean;
begin
  Called := False;
  WhenTrue := function: Integer
    begin
      Result := 1;
      Called := True;
    end;

  Result := &If<Integer>.New(1).Map(function(const Value: Integer): Context<Boolean>
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue);

  Assert.AreEqual(0, Result);
  Assert.AreEqual(False, Called);
end;

procedure TFunctionalTests.IfThenMonadWhenTrue;
var
  WhenTrue: Context<Integer>.FunctorFunc<Integer>;
  Result: Integer;
begin
  WhenTrue := function(const Value: Integer): Integer
    begin
      Result := 1;
    end;

  Result := &If<Integer>.New(100).Map(function(const Value: Integer): Context<Boolean>
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue, 0);

  Assert.AreEqual(1, Result);
end;

procedure TFunctionalTests.IfThenMonadWhenTrue2;
var
  WhenTrue: Context<Integer>.FunctorFunc<Integer>;
  Result: Integer;
begin
  WhenTrue := function(const Value: Integer): Integer
    begin
      Result := 1;
    end;

  Result := &If<Integer>.New(100).Map(function(const Value: Integer): Context<Boolean>
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue);

  Assert.AreEqual(1, Result);
end;

procedure TFunctionalTests.IfThenMonadWhenTrue3;
var
  WhenTrue: Context<Integer>;
  Result: Integer;
begin
  WhenTrue := function: Integer
    begin
      Result := 1;
    end;

  Result := &If<Integer>.New(100).Map(function(const Value: Integer): Context<Boolean>
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue, 0);

  Assert.AreEqual(1, Result);
end;

procedure TFunctionalTests.IfThenMonadWhenTrue4;
var
  WhenTrue: Context<Integer>;
  Result: Integer;
begin
  WhenTrue := function: Integer
    begin
      Result := 1;
    end;

  Result := &If<Integer>.New(100).Map(function(const Value: Integer): Context<Boolean>
    begin
      Result := Value > 5;
    end).&Then<Integer>(WhenTrue);

  Assert.AreEqual(1, Result);
end;

procedure TFunctionalTests.VoidMapFuncFuncToFunctorFunc;
var
  FunctorFunc: Context<Void>.FunctorFunc<Integer>;
begin
  FunctorFunc := Void.MapFunc<Integer>(function: Integer
    begin
      Result := 100;
    end);

  Assert.AreEqual<Integer>(100, FunctorFunc(Void.Get));
end;

procedure TFunctionalTests.VoidMapFuncFunctorFuncToFunc;
var
  FunctorFunc: Context<Void>.FunctorFunc<Integer>;
begin
  FunctorFunc := function(const Value: Void): Integer
    begin
      Result := 100;
    end;

  Assert.AreEqual<Integer>(100, Void.MapFunc<Integer>(FunctorFunc)());
end;

procedure TFunctionalTests.VoidMapProcFunctorFuncToFunctorProc;
var
  FunctorProc: Context<string>.FunctorProc;
  Count: Integer;
begin
  Count := 0;
  FunctorProc := Void.MapProc<string>(function(const Value: string): Void
    begin
      Inc(Count);
    end);

  FunctorProc('A');

  Assert.AreEqual(1, Count);
end;

{ TGateway }

function TGateway.DoRun(const Index: Integer): Integer;
begin
  Result := Index * 2;
end;

function TGateway.Run(const Index: Integer): Context<Integer>;
begin
  Result := Context<Integer>.New(Index).Map<Integer>(DoRun);
end;

{ TRepo }

constructor TRepo.Create(const Gateway: IGateway);
begin
  inherited Create;

  FGateway := Gateway;
end;

function TRepo.DoRunGateway(const Index: Integer): Context<Integer>;
begin
  Result := FGateway.Run(Index);
end;

function TRepo.DoRun(const Index: Integer): Context<Integer>;
begin
  Result := Context<Integer>.New(Index * 2).Map<Integer>(DoRunGateway);
end;

function TRepo.Run(const Index: Integer): Context<Integer>;
begin
  Result := Context<Integer>.New(Index).Map<Integer>(DoRun);
end;

initialization
  TDUnitX.RegisterTestFixture(TFunctionalTests);
end.
