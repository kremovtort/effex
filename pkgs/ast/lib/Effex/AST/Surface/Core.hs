module Effex.AST.Surface.Core where

import "base" GHC.Generics (Generic)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)

import Effex.AST.Surface.Common (Identifier, Pub, RegionIdentifier, UnitF)
import Effex.AST.Surface.Literal (Literal)
import Effex.AST.Surface.Operators (BinOp, UnOp)
import Effex.AST.Surface.Pattern (Pattern, PatternSimple)
import Effex.AST.Surface.Type (
  BindersWoConstraintsF,
  FnEffectsResultF,
  KindTreeRootF,
  QualifiedIdentifierF,
  Type,
  TypeArgumentsF,
  TypeDefinitionF,
  WhereBlockF,
 )
import Effex.AST.Surface.Use (UseClause)

--- Statements

data Statement ann = Statement
  { stmt :: StatementF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data StatementF ann
  = SLetF (LetDefinitionF ann)
  | SAssignF (AssignStatementF ann)
  | SReturnF (Expression ann) ann
  | SContinueF (Maybe (Identifier ann)) ann
  | SBreakF (Maybe (Identifier ann)) ann
  | SMatchF (MatchExpressionF ann)
  | SIfF (IfExpressionF ann)
  | SLoopF (LoopExpressionF ann)
  | SWhileF (WhileStatementF ann)
  | SForF (ForStatementF ann)
  | SExpressionF (Expression ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LetDefinitionF ann = LetDefinitionF
  { lhs :: PatternSimple ann
  , lhsType :: Maybe (Type ann)
  , rhs :: Expression ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data AssignStatementF ann = AssignStatementF
  { lhs :: LHSExpression ann
  , rhs :: Expression ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WhileStatementF ann = WhileStatementF
  { label :: Maybe (Identifier ann)
  , condition :: ConditionF ann
  , body :: CodeBlockF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ForStatementF ann = ForStatementF
  { label :: Maybe (Identifier ann, ann)
  , pattern :: PatternSimple ann
  , iterable :: Expression ann
  , body :: CodeBlockF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LHSExpression ann = LHSExpression
  { lhs :: LHSExpressionF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LHSExpressionF ann
  = LHSEWildcard
  | LHSEVar (Identifier ann)
  | LHSEIndex (LHSExpression ann) (Expression ann)
  | LHSEDotAccess (LHSExpression ann) (Identifier ann)
  | LHSEUnOp (LHSUnOpExpression ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

newtype LHSUnOpExpression ann
  = LHSUnOpExpressionDeref (Expression ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (ToExpr)

--- Expressions

data Expression ann = Expression
  { expr :: ExpressionF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ExpressionF ann
  = ELiteral Literal -- 0 | true | "str"
  | EOfType (Expression ann) (Type ann) -- expr : T
  | EParens (Expression ann) -- (expr)
  | EIdent (QualifiedIdentifierF ann) -- ident | someModule::ident
  | EIndex (Expression ann) (Expression ann) -- expr[index]
  | EDotAccess (Expression ann) (QualifiedIdentifierF ann) -- expr.ident
  | EUnOpF (UnOpExpression ann) -- -a | !a | *a | &a | &mut a | &'s mut a
  | EBinOpF (BinOpExpression ann) -- a * b | a + b | a ++ b | etc
  | EAppF (FnAppF ann) -- f(a, b, c) | f(a, b, c) with {}
  | EWithBlockF (WithBlockF ann) -- with { let a = b; c = d } in { ... }
  | ETupleF (Expression ann) (NonEmptyVector (Expression ann)) -- (a, b, c)
  | EMatchF (MatchExpressionF ann) -- match expr { Pattern => expr, ... }
  | EIfF (IfExpressionF ann) -- if expr { then_ } else { else_ }
  | ELoopF (LoopExpressionF ann) -- loop { ... } | 'label: loop { ... }
  | EBlockF (CodeBlockF ann) -- { ... }
  | EAllocF (AllocF ann) -- alloc 'into { ... }
  | EHandleF (HandleExpressionF ann) -- handle Effect
  | ELambdaF (LambdaF ann) -- <A>|a: T, b: T| -> T where Monoid<T> { a ++ B }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data UnOpExpression ann = UnOpExpression
  { op :: UnOp ann
  , operand :: Expression ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data BinOpExpression ann = BinOpExpression
  { op :: BinOp ann
  , left :: Expression ann
  , right :: Expression ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LambdaF ann
  = LamShortF (LambdaShortF ann)
  | LamFullF (LambdaFullF ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LambdaShortF ann = LambdaShortF
  { args :: Vector (LambdaArgF ann)
  , body :: Expression ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LambdaFullF ann = LambdaFullF
  { typeParams :: Maybe (BindersWoConstraintsF ann)
  , args :: Vector (LambdaArgF ann)
  , effectsResult :: Maybe (FnEffectsResultF ann)
  , whereBlock :: Maybe (WhereBlockF ann)
  , body :: CodeBlockF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LambdaArgF ann = LambdaArgF
  { mut :: Maybe ann
  , name :: Identifier ann
  , type_ :: Maybe (Type ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data HandleExpressionF ann = HandleExpressionF
  { effects :: NonEmptyVector (Type ann)
  , in_ :: Maybe (Type ann)
  , returning :: Maybe (HandleReturningF ann)
  , body :: HandleBodyF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data HandleReturningF ann = HandleReturningF
  { binder :: Identifier ann
  , result :: Type ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data HandleBodyF ann = HandleBodyF
  { uses :: Vector (UseClause ann)
  , items :: NonEmptyVector (EffectItemDefinitionF ann, ann)
  , returning :: Maybe (HandleReturningBlockF ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectItemDefinitionF ann
  = EDefinitionLetF (LetDefinitionF ann)
  | EDefinitionOpF (OpDefinitionF ann)
  | EDefinitionOpInfixF (OpInfixDefinitionF ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data HandleReturningBlockF ann = HandleReturningBlockF
  { arg :: Identifier ann
  , argType :: Identifier ann
  , body :: CodeBlockF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FnAppF ann = FnAppF
  { callee :: Expression ann
  , typeParams :: Maybe (TypeArgumentsF ann)
  , args :: FnAppArgsF ann
  , with :: Maybe (FnWithAppF ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FnAppArgsF ann
  = FnAppArgsUnnamedF (Vector (Expression ann))
  | FnAppArgsNamedF (Vector (ArgNamedF ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ArgNamedF ann = ArgNamedF
  { name :: Identifier ann
  , value :: Maybe (Expression ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data AllocF ann = AllocF
  { into :: Maybe (RegionIdentifier ann)
  , body :: CodeBlockF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data CodeBlockF ann = CodeBlockF
  { region :: Maybe (RegionIdentifier ann)
  , uses :: Vector (UseClause ann)
  , statements :: Vector (Statement ann)
  , result :: Maybe (Expression ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data MatchExpressionF ann = MatchExpressionF
  { value :: Expression ann
  , arms :: NonEmptyVector (MatchArmF ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data MatchArmF ann = MatchArmF
  { pattern :: Pattern ann
  , guard :: Maybe (Expression ann)
  , expression :: Expression ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data IfExpressionF ann = IfExpressionF
  { branches :: NonEmptyVector (IfBranchF ann)
  , else_ :: Maybe (CodeBlockF ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data IfBranchF ann = IfBranchF
  { condition :: ConditionF ann
  , result :: CodeBlockF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ConditionF ann
  = CondBoolF (Expression ann)
  | CondLetF (LetConditionF ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LetConditionF ann = LetConditionF
  { pattern :: Pattern ann
  , patternExpr :: Expression ann
  , bool :: Maybe (Expression ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LoopExpressionF ann = LoopExpressionF
  { label :: Maybe (Identifier ann)
  , body :: CodeBlockF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithBlockF ann = WithBlockF
  { withStatements :: NonEmptyVector (WithStatementF ann)
  , block :: CodeBlockF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithStatementF ann
  = WthStmtLetHandleF (EffHandleRhsF ann) -- let! take_error(|e| handle_error(e));
  | WthStmtLetLabelledHandleF (NonEmptyVector (EffLabelTyF ann)) (EffHandleRhsF ann)
  | WthStmtLetAssignF (EffLabelTyF ann) (EffLabelTyF ann)
  | WthStmtLabelledHandleF (NonEmptyVector (EffLabelTyF ann)) (EffHandleRhsF ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FnWithAppF ann = FnWithAppF
  { clauses :: NonEmptyVector (WithAppClauseF ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithAppClauseF ann
  = WthApClauseAssignF (EffLabelTyF ann) (EffLabelTyF ann)
  | WthApClauseHandleF (NonEmptyVector (EffLabelTyF ann)) (EffHandleRhsF ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffLabelTyF ann = EffLabelTyF
  { name :: Identifier ann
  , ty :: Maybe (Type ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffHandleRhsF ann = EffHandleRhsF
  { expr :: Expression ann
  , in_ :: Maybe (NonEmptyVector (InStatementF ann))
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data InStatementF ann
  = InStatementAssignF (Type ann) (EffLabelTyF ann)
  | InStatementHandleF (NonEmptyVector (Type ann)) (EffHandleRhsF ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

--- Callables

data CallKind = KFn | KOp
  deriving (Eq, Ord, Show, Generic, ToExpr)

-- | Receiver header for infix calls
data ReceiverHeaderF ann = ReceiverHeaderF
  { typeParams :: Maybe (BindersWoConstraintsF ann)
  , name :: Identifier ann
  , type_ :: Type ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

-- | Common header for all callable entities
data CallableHeader reciever name ann = CallableHeader
  { receiver :: reciever ann
  , name :: name ann
  , typeParams :: Maybe (BindersWoConstraintsF ann)
  , args :: Vector (ArgF ann)
  , effectsResult :: Maybe (FnEffectsResultF ann)
  , whereBlock :: Maybe (WhereBlockF ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ArgF ann = ArgF
  { mut :: Maybe ann
  , name :: Identifier ann
  , type_ :: Type ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data
  CallableF
    (kind :: CallKind)
    reciever
    name
    body
    ann
  = CallableF
  { header :: CallableHeader reciever name ann
  , body :: body ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

type FnDeclarationF =
  CallableF
    'KFn
    UnitF
    Identifier
    UnitF

type FnInfixDeclarationF =
  CallableF
    'KFn
    ReceiverHeaderF
    Identifier
    UnitF

type OpDeclarationF =
  CallableF
    'KOp
    UnitF
    Identifier
    UnitF

type OpInfixDeclarationF =
  CallableF
    'KOp
    ReceiverHeaderF
    Identifier
    UnitF

type FnDefinitionF =
  CallableF
    'KFn
    UnitF
    Identifier
    CodeBlockF

type FnInfixDefinitionF =
  CallableF
    'KFn
    ReceiverHeaderF
    Identifier
    CodeBlockF

type OpDefinitionF =
  CallableF
    'KOp
    UnitF
    QualifiedIdentifierF
    CodeBlockF

type OpInfixDefinitionF =
  CallableF
    'KOp
    ReceiverHeaderF
    QualifiedIdentifierF
    CodeBlockF

--- Declarations
--
data TraitF ann = TraitF
  { sealed :: Bool
  , name :: Identifier ann
  , typeParams :: BindersWoConstraintsF ann
  , superTraits :: Maybe (NonEmptyVector (Type ann))
  , traitBody :: Vector (TraitItemF ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TraitItemF ann = TraitItemF
  { pub :: Maybe (Pub ann)
  , item :: TraitItemVariantF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TraitItemVariantF ann
  = TItemVarTypeDeclarationF (TypeDeclarationF ann)
  | TItemVarLetDeclarationF (LetDeclarationF ann)
  | TItemVarFnDeclarationF (FnDeclarationF ann)
  | TItemVarFnInfixDeclarationF (FnInfixDeclarationF ann)
  | TItemVarFnDefinitionF (FnDefinitionF ann)
  | TItemVarFnInfixDefinitionF (FnInfixDefinitionF ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ImplF ann = ImplF
  { implParams :: Maybe (BindersWoConstraintsF ann)
  , trait :: QualifiedIdentifierF ann
  , traitParams :: TypeArgumentsF ann
  , whereBlock :: Maybe (WhereBlockF ann)
  , body :: Vector (ImplItemVariantF ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ImplItemVariantF ann
  = IItemVarTypeF (TypeDefinitionF ann)
  | IItemVarLetF (LetDefinitionF ann)
  | IItemVarFnF (FnDefinitionF ann)
  | IItemVarFnInfixF (FnInfixDefinitionF ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectF ann = EffectF
  { sealed :: Bool
  , name :: Identifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ann)
  , superEffects :: Maybe (NonEmptyVector (Type ann))
  , whereBlock :: Maybe (WhereBlockF ann)
  , effectBody :: Vector (EffectItemF ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectItemF ann = EffectItemF
  { pub :: Maybe (Pub ann)
  , item :: EffectItemVariantF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectItemVariantF ann
  = EItemVarTypeDeclarationF (TypeDeclarationF ann)
  | EItemVarLetDeclarationF (LetDeclarationF ann)
  | EItemVarOpDeclarationF (OpDeclarationF ann)
  | EItemVarOpInfixDeclarationF (OpInfixDeclarationF ann)
  | EItemVarOpDefinitionF (OpDefinitionF ann)
  | EItemVarOpInfixDefinitionF (OpInfixDefinitionF ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TypeDeclarationF ann = TypeDeclarationF
  { name :: Identifier ann
  , kindShort :: Maybe (KindTreeRootF ann, ann)
  , type_ :: Maybe (Type ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LetDeclarationF ann = LetDeclarationF
  { name :: Identifier ann
  , type_ :: Type ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
