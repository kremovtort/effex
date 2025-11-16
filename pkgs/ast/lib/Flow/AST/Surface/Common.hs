{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Flow.AST.Surface.Common where

import "base" GHC.Generics (Generic)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "text" Data.Text (Text)
import "tree-diff" Data.TreeDiff.Class (ToExpr (toExpr))

data Identifier ann = Identifier
  { name :: Text
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data RegionIdentifier ann = RegionIdentifier
  { name :: Text
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

instance (ToExpr a) => ToExpr (NonEmptyVector a) where
  toExpr = toExpr . NonEmptyVector.toList

data Pub ann
  = PubPub
  | PubPackage ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
