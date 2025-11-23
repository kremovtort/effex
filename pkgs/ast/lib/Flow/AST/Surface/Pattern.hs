module Flow.AST.Surface.Pattern where

import "base" GHC.Generics (Generic)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "tree-diff" Data.TreeDiff.Class (ToExpr)

import Flow.AST.Surface.Common (Identifier)
import Flow.AST.Surface.Literal (Literal)
import Flow.AST.Surface.Type (BindersWoConstraintsF, QualifiedIdentifierF, Type)

data Pattern ann = Pattern
  { pat :: PatternF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternSimple ann = PatternSimple
  { pat :: PatternSimpleF PatternSimple ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternF ann
  = PatSimpleF (PatternSimpleF Pattern ann)
  | PatLiteralF Literal
  | PatOrF (NonEmptyVector (PatternSimpleF Pattern ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternSimpleF pat ann
  = PatSimWildcardF
  | PatSimVarF (PatternVarF ann)
  | PatSimTupleF (NonEmptyVector (pat ann))
  | PatSimConstructorAppF (PatternConsturctorAppF pat ann)
  | PatSimConstructorF (QualifiedIdentifierF ann)
  | PatSimOfTypeF (pat ann) (Type ann)
  | PatSimAsF (pat ann) (PatternVarF ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternVarF ann = PatternVarF
  { ref :: Maybe ann
  , mut :: Maybe ann
  , name :: Identifier ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternConsturctorAppF pat ann = PatternConsturctorAppF
  { name :: QualifiedIdentifierF ann
  , typeParams :: Maybe (BindersWoConstraintsF ann)
  , fields :: PatternFieldsF pat ann
  , fieldsAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldsF pat ann
  = PatFldsUnnamedF (NonEmptyVector (PatternFieldUnnamedF pat ann))
  | PatFldsNamedF (NonEmptyVector (PatternFieldNamedF pat ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldUnnamedF pat ann = PatternFieldUnnamedF
  { value :: pat ann
  , optional :: Maybe ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldNamedF pat ann
  = PatFldNmdValueF (PatternFieldNamedValueF pat ann)
  | PatFldNmdPunningF (PatternFieldNamedPunningF pat ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldNamedValueF pat ann = PatternFieldNamedValueF
  { name :: Identifier ann
  , value :: pat ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldNamedPunningF pat ann = PatternFieldNamedPunningF
  { ref :: Maybe ann
  , mut :: Maybe ann
  , name :: Identifier ann
  , optional :: Maybe ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
