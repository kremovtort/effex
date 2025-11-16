{-# LANGUAGE TypeFamilies #-}

module Flow.AST.Surface.Constraint where

import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Data.Vector.NonEmpty (NonEmptyVector)
import Flow.AST.Surface.Common (RegionIdentifier, Identifier)

data QualifiedIdentifierF ty ann = QualifiedIdentifierF
  { qualifierPrefix :: Maybe (QualifierPrefixF ann)
  , qualifier :: Maybe (NonEmptyVector (Identifier ann))
  , typeQualifier :: Maybe (TypeQualifierF ty ann)
  , identifier :: Identifier ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data QualifierPrefixF ann
  = QlfrPrfxSelf ann
  | QlfrPrfxSupers (NonEmptyVector ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TypeQualifierF ty ann = TypeQualifierF
  { typeName :: Identifier ann
  , typeParams :: BindersAppF ty ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data BindersAppF ty ann = BindersAppF
  { types :: NonEmptyVector (ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data BindersF regionBinder typeBinder ty ann = BindersF
  { regions :: Vector (regionBinder ty ann)
  , types :: Vector (typeBinder ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

type BindersWoConstraintsF = BindersF RegionBinderWoConstraintsF BinderWoConstraintsF

newtype BinderAppF ty ann = BinderAppF {ty :: ty ann}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (ToExpr)

newtype RegionBinderWoConstraintsF ty ann
  = RegionBinderWoConstraintsF (RegionIdentifier ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (ToExpr)

data BinderWoConstraintsF ty ann = BinderWoConstraintF
  { name :: Identifier ann
  , kindShort :: Maybe (KindTreeRootF ty ann)
  , typeType :: Maybe (ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TypeDefinitionF ty ann = TypeDefinitionF
  { name :: Identifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , type_ :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WhereClauseF ty ann -- type X = Y | Functor<A> | etc
  = WhereConstraintF (ty ann)
  | WhereAliasF (TypeDefinitionF ty ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WhereBlockF ty ann = WhereBlockF
  { clauses :: NonEmptyVector (WhereClauseF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

type KindTreeRootF ty ann = NonEmptyVector (KindTreeF ty ann) -- <_, _<_>>

data KindTreeF ty ann
  = KTHoleF (KindHoleF ty ann) -- _ | _: Type
  | KTParamsF (KindParamsF ty ann) -- _<_, _>
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data KindHoleF ty ann = KindHoleF
  { holeAnn :: ann
  , typeType :: Maybe (ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data KindParamsF ty ann = KindParamsF
  { holeAnn :: ann
  , params :: NonEmptyVector (KindTreeF ty ann)
  , typeType :: Maybe (ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
