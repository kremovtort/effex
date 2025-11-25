module Effex.AST.Surface.Use where

import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)

import Effex.AST.Surface.Common (Identifier)
import Data.Vector.NonEmpty (NonEmptyVector)

data UseClause ann = UseClause
  { root :: UseClauseRoot ann
  , tree :: UseTree ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data UseClauseRoot ann
  = UsClSelf ann
  | UsClSupers (NonEmptyVector ann)
  | UsClPackage (Identifier ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data UseTree ann
  = UseTrBranch (Identifier ann) (UseTree ann)
  | UseTrNested (Vector (UseTree ann))
  | UseTrLeafWildcard ann
  | UseTrLeafIdent (UseTreeLeaf Identifier ann)
  | UseTrLeafMethod (UseTreeLeaf Identifier ann)
  | UseTrLeafMethodAsFn (UseTreeLeafMethodAsFn ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data UseTreeLeaf f ann = UseTreeLeaf
  { use :: f ann
  , as :: Maybe (f ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data UseTreeLeafMethodAsFn ann = UseTreeLeafMethodAsFn
  { use :: Identifier ann
  , as :: Identifier ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
