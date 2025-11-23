module Flow.AST.Surface.Pattern where

import "base" GHC.Generics (Generic)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "base" Prelude hiding (Enum)

import Flow.AST.Surface.Common (Identifier)
import Flow.AST.Surface.Constraint (BindersWoConstraintsF, QualifiedIdentifierF)
import Flow.AST.Surface.Literal (Literal)

data PatternF pat ty ann
  = PatSimpleF (PatternSimpleF pat ty ann)
  | PatLiteralF Literal
  | PatOrF (NonEmptyVector (PatternSimpleF pat ty ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternSimpleF pat ty ann
  = PatSimWildcardF
  | PatSimVarF (PatternVarF ann)
  | PatSimTupleF (NonEmptyVector (pat ann))
  | PatSimConstructorAppF (PatternConsturctorAppF pat ty ann)
  | PatSimConstructorF (QualifiedIdentifierF ty ann)
  | PatSimOfTypeF (pat ann) (ty ann)
  | PatSimAsF (pat ann) (PatternVarF ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternVarF ann = PatternVarF
  { ref :: Maybe ann
  , mut :: Maybe ann
  , name :: Identifier ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternConsturctorAppF pat ty ann = PatternConsturctorAppF
  { name :: QualifiedIdentifierF ty ann
  , typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , fields :: PatternFieldsF pat ty ann
  , fieldsAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldsF pat ty ann
  = PatFldsUnnamedF (NonEmptyVector (PatternFieldUnnamedF pat ty ann))
  | PatFldsNamedF (NonEmptyVector (PatternFieldNamedF pat ty ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldUnnamedF pat ty ann = PatternFieldUnnamedF
  { value :: pat ann
  , optional :: Maybe ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldNamedF pat ty ann
  = PatFldNmdValueF (PatternFieldNamedValueF pat ty ann)
  | PatFldNmdPunningF (PatternFieldNamedPunningF pat ty ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldNamedValueF pat ty ann = PatternFieldNamedValueF
  { name :: Identifier ann
  , value :: pat ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldNamedPunningF pat ty ann = PatternFieldNamedPunningF
  { ref :: Maybe ann
  , mut :: Maybe ann
  , name :: Identifier ann
  , optional :: Maybe ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
