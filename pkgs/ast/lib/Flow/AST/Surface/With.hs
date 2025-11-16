module Flow.AST.Surface.With where

import "base" GHC.Generics (Generic)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "base" Prelude hiding (Enum)

import Flow.AST.Surface.Common (Identifier)
import Flow.AST.Surface.Syntax (CodeBlockF)

data WithAppF ty expr ann = WithAppF
  { clauses :: NonEmptyVector (WithAppClauseF ty expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithAppClauseF ty expr ann
  = WthApClauseAssignF (EffLabelTyF ty ann) (EffLabelTyF ty ann)
  | WthApClauseHandleF (NonEmptyVector (EffLabelTyF ty ann)) (EffHandleRhsF ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithBlockF stmt ty expr ann = WithBlockF
  { withStatements :: NonEmptyVector (WithStatementF ty expr ann)
  , block :: CodeBlockF stmt expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithStatementF ty expr ann
  = WthStmtLetHandleF (EffHandleRhsF ty expr ann) -- let! take_error(|e| handle_error(e));
  | WthStmtLetLabelledHandleF (NonEmptyVector (EffLabelTyF ty ann)) (EffHandleRhsF ty expr ann)
  | WthStmtLetAssignF (EffLabelTyF ty ann) (EffLabelTyF ty ann)
  | WthStmtLabelledHandleF (NonEmptyVector (EffLabelTyF ty ann)) (EffHandleRhsF ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffLabelTyF ty ann = EffLabelTyF
  { name :: Identifier ann
  , ty :: Maybe (ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffHandleRhsF ty expr ann = EffHandleRhsF
  { expr :: expr ann
  , in_ :: Maybe (NonEmptyVector (InStatementF ty expr ann))
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data InStatementF ty expr ann
  = InStatementAssignF (ty ann) (EffLabelTyF ty ann)
  | InStatementHandleF (NonEmptyVector (ty ann)) (EffHandleRhsF ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
