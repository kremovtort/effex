module Flow.AST.Surface.Data where

import "base" GHC.Generics (Generic)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)

import Flow.AST.Surface.Common (Identifier, Pub)
import Flow.AST.Surface.Type (BindersWoConstraintsF, Type, WhereBlockF)

data StructF ann = StructF
  { name :: Identifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ann)
  , whereBlock :: Maybe (WhereBlockF ann)
  , fields :: FieldsDeclF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FieldsDeclF ann
  = FieldsDeclNamedF (Vector (FieldDeclF ann))
  | FieldsDeclTupleF (Vector (Type ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FieldDeclF ann = FieldDeclF
  { pub :: Maybe (Pub ann)
  , name :: Identifier ann
  , type_ :: Type ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumF ann = EnumF
  { name :: Identifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ann)
  , whereBlock :: Maybe (WhereBlockF ann)
  , variants :: EnumVariantsF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumVariantsF ann
  = EVariantsSimpleF (NonEmptyVector (EnumVariantF ann))
  | EVariantsGeneralized (NonEmptyVector (EnumVariantGeneralizedF ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumVariantF ann = EnumVariantF
  { name :: Identifier ann
  , fields :: Maybe (FieldsDeclF ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumVariantGeneralizedF ann = EnumVariantGeneralizedF
  { name :: Identifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ann)
  , fields :: Maybe (FieldsDeclF ann)
  , result :: Type ann
  , whereBlock :: Maybe (WhereBlockF ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumVariantGeneralizedSimpleF ann = EnumVariantGeneralizedSimpleF
  { enumVariant :: EnumVariantF ann
  , type_ :: Type ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
