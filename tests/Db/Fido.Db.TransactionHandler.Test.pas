unit Fido.Db.TransactionHandler.Test;

interface

uses
  Fido.Db.Transaction.Handler.Intf,
  Fido.Db.Transaction.Handler.Base;

type
  TTestTransactionHandler = class (TBaseTransactionHandler, ITransactionHandler)
  private
    FNestingLevel: Integer;
    FNestedTransactionRollbacked: Boolean;
    function NestedTransactionRollbacked: Boolean;
    function NestingLevel: Integer;
  protected
    procedure DoStart; override;
    procedure DoCommit; override;
    procedure DoRollback; override;
    function DoGetNestedTransactionRollbacked: Boolean; override;
    function DoGetNestingLevel: Integer; override;
  public
    procedure AfterConstruction; override;
  end;

implementation

{ TTestTransactionHandler }

procedure TTestTransactionHandler.AfterConstruction;
begin
  inherited;
  FNestingLevel := 0;
  FNestedTransactionRollbacked := False;
end;

procedure TTestTransactionHandler.DoCommit;
begin
  inherited;
  Dec(FNestingLevel);
  if FNestingLevel = 0 then
    FNestedTransactionRollbacked := False;
end;

function TTestTransactionHandler.DoGetNestedTransactionRollbacked: Boolean;
begin
  Result := FNestedTransactionRollbacked;
end;

function TTestTransactionHandler.DoGetNestingLevel: Integer;
begin
  Result := FNestingLevel;
end;

procedure TTestTransactionHandler.DoRollback;
begin
  inherited;
  Dec(FNestingLevel);
  FNestedTransactionRollbacked := FNestingLevel > 0;
end;

procedure TTestTransactionHandler.DoStart;
begin
  inherited;
  Inc(FNestingLevel);
end;

function TTestTransactionHandler.NestedTransactionRollbacked: Boolean;
begin
  Result := FNestedTransactionRollbacked;
end;

function TTestTransactionHandler.NestingLevel: Integer;
begin
  Result := FNestingLevel;
end;

end.
