module Effex.Parser.Literal where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "containers" Data.Set qualified as Set

import Effex.AST.Surface.Literal qualified as Surface
import Effex.Lexer qualified as Lexer
import Effex.Parser.Common (Parser, SourceSpan, token, single)

literal :: Parser (Surface.Literal, SourceSpan)
literal = Megaparsec.choice [unit, bool, integer, float, byte, byteString, char, string]

unit :: Parser (Surface.Literal, SourceSpan)
unit = do
  tok <- single (Lexer.Punctuation Lexer.LeftRightParen)
  pure (Surface.LitUnit, tok.span)

bool :: Parser (Surface.Literal, SourceSpan)
bool = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "bool literal")
      \case
          Lexer.BoolLiteral b -> Just b
          _ -> Nothing
  pure (Surface.LitBool tok.value, tok.span)

integer :: Parser (Surface.Literal, SourceSpan)
integer = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "integer literal")
      \case
          Lexer.IntegerLiteral i -> Just i
          _ -> Nothing
  pure (Surface.LitInteger tok.value, tok.span)

float :: Parser (Surface.Literal, SourceSpan)
float = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "float literal")
      \case
          Lexer.FloatLiteral f -> Just f
          _ -> Nothing
  pure (Surface.LitFloat tok.value, tok.span)

byte :: Parser (Surface.Literal, SourceSpan)
byte = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "byte literal")
      \case
          Lexer.ByteLiteral b -> Just b
          _ -> Nothing
  pure (Surface.LitByte tok.value, tok.span)

byteString :: Parser (Surface.Literal, SourceSpan)
byteString = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "bytestring literal")
      \case
          Lexer.ByteStringLiteral b -> Just b
          _ -> Nothing
  pure (Surface.LitByteString tok.value, tok.span)

char :: Parser (Surface.Literal, SourceSpan)
char = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "char literal")
      \case
          Lexer.CharLiteral c -> Just c
          _ -> Nothing
  pure (Surface.LitChar tok.value, tok.span)

string :: Parser (Surface.Literal, SourceSpan)
string = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "string literal")
      \case
          Lexer.StringLiteral s -> Just s
          _ -> Nothing
  pure (Surface.LitString tok.value, tok.span)
