module Effex.AST.Surface.Literal where

import "base" Data.Word (Word8)
import "base" GHC.Generics (Generic)
import "bytestring" Data.ByteString (ByteString)
import "scientific" Data.Scientific (Scientific)
import "text" Data.Text (Text)
import "tree-diff" Data.TreeDiff.Class (ToExpr)

import Effex.AST.Surface.Common ()

data Literal
  = LitUnit
  | LitBool Bool
  | LitInteger Integer
  | LitFloat Scientific
  | LitByte Word8
  | LitByteString ByteString
  | LitChar Char
  | LitString Text
  deriving (Eq, Ord, Show, Generic, ToExpr)
