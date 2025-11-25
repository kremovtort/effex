module Effex.Core.AST.Expr where

import Effex.Core.AST.Literal (Literal)
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector (Vector)

data ESymbol -- type for identifiers and symbols of expressions

data SimpleVarIdentifier

data AnyVarIdentifier ty ann

data Type

data WithStatementF ty expr ann = WithStatementF

data Args
  = ArgsUnnamed (Vector ESymbol)
  | ArgsNamed (Vector (SimpleVarIdentifier, ESymbol))

data App ty expr ann = App
  { callee :: ESymbol
  , typeParams :: Maybe (NonEmptyVector (ty ann))
  , args :: Args
  , with :: Maybe (NonEmptyVector (WithStatementF ty expr ann))
  }

data ExpressionF stmt ty expr ann
  = ELiteral Literal
  | EDotAccessF ESymbol (AnyVarIdentifier ty ann) -- x.y
  | EAppF (App ty expr ann)
  | EBlockF (CodeBlockF stmt ty expr ann)
  | EWithF (WithF stmt ty expr ann)

data WithF stmt ty expr ann = WithF

data CodeBlockF stmt ty expr ann = CodeBlock
  { statements :: Vector (stmt ann)
  , result :: Maybe (expr ann)
  , ann :: ann
  }
