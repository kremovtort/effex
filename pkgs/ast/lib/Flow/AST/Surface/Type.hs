module Flow.AST.Surface.Type where

import "base" GHC.Generics (Generic)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)

import Flow.AST.Surface.Common (Identifier, RegionIdentifier)

--- Types

data Type ann = Type
  { ty :: TypeF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TypeF ann
  = TyWildcardF -- _
  | TyRegionF (RegionIdentifier ann) -- 's
  | TyBuiltinF Builtin
  | TyIdentifierF (QualifiedIdentifierF ann) -- MyType
  | TyParensF (Type ann) -- (A)
  | TyAppF (AppF ann) -- Option<A>
  | TyTupleF (NonEmptyVector (Type ann)) -- (A, B, C)
  | TyRefAppF (RefF ann) (Type ann) -- &'s T | &'s mut T | &T | &mut T
  | TyRefF (RefF ann) -- &'s | &'s mut | & | &mut / higher kind reference
  | TyForallF (ForallF ann) -- <A :< Monoid> fn(List<A>) -> A
  | TyFnF (FnF ann) -- fn(List<A>) -> A
  | TyEffectRowF (EffectRowF ann) -- @[IO, State<S>] | @['s, IO] | @['s, IO, s: State<S>, ..R] | etc
  | TyEquals (Type ann) (Type ann) -- A == B
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data Builtin
  = BuiltinUnit
  | BuiltinNever
  deriving (Eq, Ord, Show, Generic, ToExpr)

data AppF ann = AppF -- Option<A>
  { head :: Type ann
  , args :: TypeArgumentsF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data RefF ann = RefF -- &'s T | &'s mut T | &T | &mut T
  { region :: Maybe (RegionIdentifier ann)
  , mutability :: Maybe ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ForallF ann = ForallF -- <A :< Monoid> fn(List<A>) -> A
  { params :: BindersWoConstraintsF ann
  , result :: Type ann
  , whereBlock :: Maybe (WhereBlockF ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FnF ann = FnF -- fn(List<A>) -> A
  { args :: Vector (Type ann)
  , effectsResult :: Maybe (FnEffectsResultF ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FnEffectsResultF ann = FnEffectsResultF
  { effects :: Maybe (FnEffectsF ann)
  , result :: Type ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FnEffectsF ann
  = FnEffectsTypeF (Type ann)
  | FnEffectsRowF (FnEffectRowF ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FnEffectRowF ann = FnEffectRowF
  { regions :: Vector (RegionIdentifier ann)
  , effects :: Vector (FnEffectAtomF ann)
  , tailVars :: Vector (Type ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FnEffectAtomF ann
  = FnEffectAtomTypeF (Type ann) -- E1
  | FnEffectAtomNameTypeF (Identifier ann) (Type ann) -- e1: E1
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectRowF ann = EffectRowF
  { effects :: Vector (Type ann)
  , tailVars :: Vector (Type ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TypeDefinitionF ann = TypeDefinitionF
  { name :: Identifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ann)
  , type_ :: Type ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

--- Type binders

data TypeArgumentsF ann = TypeArgumentsF
  { types :: NonEmptyVector (Type ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data BindersF regionBinder typeBinder ann = BindersF
  { regions :: Vector (regionBinder ann)
  , types :: Vector (typeBinder ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

type BindersWoConstraintsF = BindersF RegionBinderWoConstraintsF BinderWoConstraintsF

newtype RegionBinderWoConstraintsF ann
  = RegionBinderWoConstraintsF (RegionIdentifier ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (ToExpr)

data BinderWoConstraintsF ann = BinderWoConstraintF
  { name :: Identifier ann
  , kindShort :: Maybe (KindTreeRootF ann)
  , typeType :: Maybe (Type ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

type KindTreeRootF ann = NonEmptyVector (KindTreeF ann) -- <_, _<_>>

data KindTreeF ann
  = KTHoleF (KindHoleF ann) -- _ | _: Type
  | KTParamsF (KindParamsF ann) -- _<_, _>
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data KindHoleF ann = KindHoleF
  { holeAnn :: ann
  , typeType :: Maybe (Type ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data KindParamsF ann = KindParamsF
  { holeAnn :: ann
  , params :: NonEmptyVector (KindTreeF ann)
  , typeType :: Maybe (Type ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

--- trait bounds

data WhereClauseF ann -- type X = Y | Functor<A> | etc
  = WhereConstraintF (Type ann)
  | WhereAliasF (TypeDefinitionF ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WhereBlockF ann = WhereBlockF
  { clauses :: NonEmptyVector (WhereClauseF ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

--- Identifiers

data QualifiedIdentifierF ann = QualifiedIdentifierF
  { qualifierPrefix :: Maybe (QualifierPrefixF ann)
  , qualifier :: Maybe (NonEmptyVector (Identifier ann))
  , typeQualifier :: Maybe (TypeQualifierF ann)
  , identifier :: Identifier ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data QualifierPrefixF ann
  = QlfrPrfxSelf ann
  | QlfrPrfxSupers (NonEmptyVector ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TypeQualifierF ann = TypeQualifierF
  { typeName :: Identifier ann
  , typeParams :: TypeArgumentsF ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
