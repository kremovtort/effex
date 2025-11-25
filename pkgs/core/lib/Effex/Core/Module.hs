{-# OPTIONS_GHC -Wno-orphans #-}

module Effex.Core.Module where

import Prelude hiding (readFile)

import "base" Control.Monad (unless, when)
import "base" Data.Either (partitionEithers)
import "base" Data.Foldable (toList)
import "base" Data.Traversable (for)
import "base" Data.Void (Void)
import "base" GHC.Generics (Generic)
import "base" GHC.Stack (CallStack, HasCallStack)
import "containers" Data.Sequence (Seq)
import "containers" Data.Sequence qualified as Seq
import "effectful" Effectful (Eff, (:>))
import "effectful" Effectful.Error.Static (Error, runError, runErrorWith, throwError)
import "effectful" Effectful.FileSystem (FileSystem, doesFileExist)
import "effectful" Effectful.FileSystem.IO.ByteString (readFile)
import "filepath" System.FilePath (takeDirectory, (</>))
import "hashable" Data.Hashable (Hashable (..))
import "megaparsec" Text.Megaparsec (ParseErrorBundle)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "text" Data.Text.Encoding qualified as Text.Encoding
import "text" Data.Text.Encoding.Error qualified as Text.Encoding
import "unordered-containers" Data.HashMap.Strict (HashMap)
import "unordered-containers" Data.HashSet qualified as HashSet
import "vector" Data.Vector (Vector)
import "vector" Data.Vector qualified as Vector

import Control.Monad (foldM)
import Data.TreeDiff (ToExpr)
import Effex.AST.Ann (SourceSpan)
import Effex.AST.Surface (Mod (..), ModDefinitionBody)
import Effex.AST.Surface.Common (ModuleIdentifier (..), Pub (..))
import Effex.AST.Surface.Module (
  ModDefinitionBodyF (..),
  ModF (..),
  ModuleItemF (..),
  ModuleItemVariantF (..),
 )
import Effex.Lexer qualified as Lexer
import Effex.Parser (pModDefinitionBody)

newtype PackageId = PackageId
  { name :: Text
  }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, ToExpr)

handleError ::
  (HasCallStack, Error e :> es) =>
  Eff (Error e : es) a ->
  (CallStack -> e -> Eff es a) ->
  Eff es a
handleError action handler = runErrorWith handler action

instance (Hashable a) => Hashable (Vector a) where
  hashWithSalt salt = hashWithSalt salt . toList

instance (Hashable a) => Hashable (NonEmptyVector a) where
  hashWithSalt salt = hashWithSalt salt . NonEmptyVector.toList

data Visibility
  = -- | visible to all packages
    VisibilityAll
  | -- | visible to the package modules
    VisibilityPackage
  | -- | visible to the module and its submodules
    VisibilityModule ModuleId
  deriving (Eq, Ord, Show, Hashable, Generic, ToExpr)

resolveVisibility ::
  Visibility ->
  Visibility ->
  Visibility
resolveVisibility parent current =
  case current of
    VisibilityModule modId
      | null modId.path -> VisibilityPackage
      | otherwise -> VisibilityModule modId
    VisibilityPackage -> case parent of
      VisibilityModule modId
        | null modId.path -> VisibilityPackage
        | otherwise -> VisibilityModule modId
      VisibilityPackage -> VisibilityPackage
      VisibilityAll -> VisibilityPackage
    VisibilityAll -> case parent of
      VisibilityModule modId
        | null modId.path -> VisibilityPackage
        | otherwise -> VisibilityModule modId
      VisibilityPackage -> VisibilityPackage
      VisibilityAll -> VisibilityAll

data ModuleId = ModuleId
  { package :: PackageId
  , path :: Vector Text
  }
  deriving (Eq, Ord, Show, Hashable, Generic, ToExpr)

newtype ModuleEnv = ModuleEnv
  { modules :: HashMap ModuleId ()
  }
  deriving (Eq, Ord, Show)

data BasicAnn = BasicAnn
  { filePath :: FilePath
  , span :: SourceSpan
  }
  deriving (Eq, Ord, Show, Generic, ToExpr)

data PackageKind
  = PacKLib
  | PacKExe
  deriving (Eq, Ord, Show)

type CollectPackageModulesErrors es =
  ( Error ModuleNotFoundError :> es
  , Error AmbiguousModuleLocationError :> es
  , Error DuplicateModuleDeclarationError :> es
  , ParseModuleErrors es
  )

collectPackageModules ::
  forall es.
  (HasCallStack, CollectPackageModulesErrors es, FileSystem :> es) =>
  PackageKind ->
  PackageId ->
  -- | Absolute path to the package src directory
  FilePath ->
  Eff es (Seq ModuleDefinition)
collectPackageModules packageKind packageId packageSrcDir = do
  let rootModuleId = ModuleId{package = packageId, path = Vector.empty}
  go Nothing rootModuleId
 where
  rootModulePath = case packageKind of
    PacKLib -> packageSrcDir </> "lib.fl"
    PacKExe -> packageSrcDir </> "main.fl"

  go :: Maybe BasicAnn -> ModuleId -> Eff es (Seq ModuleDefinition)
  go mDeclAnn curModId = do
    let modPaths = case NonEmptyVector.fromVector curModId.path of
          Nothing -> [rootModulePath]
          Just currentModPath' -> moduleFilePathCandidates packageSrcDir currentModPath'
    (_, asts) <-
      partitionEithers <$> for modPaths \path' -> runError @FileNotFoundError do
        ast <- readModFile path'
        pure (path', ast)
    ast <- case asts of
      [] -> throwError $ ModuleNotFoundError mDeclAnn (ModuleId{package = packageId, path = curModId.path}) modPaths
      [(_, ast)] -> pure ast
      asts' ->
        throwError $
          AmbiguousModuleLocationError
            mDeclAnn
            ( ModuleId
                { package = packageId
                , path = curModId.path
                }
            )
            (fst <$> asts')
    (defs, decls) <-
      collectModuleDefinitionModules
        ModuleDefinition
          { visibility = VisibilityModule curModId
          , identifier = curModId
          , body = ast
          , declAnn = mDeclAnn
          }
    restDefs <- mconcat <$> for (toList decls) \decl -> do
      go (Just decl.ann) decl.identifier
    pure (defs Seq.>< restDefs)

data ModuleNotFoundError = ModuleNotFoundError (Maybe BasicAnn) ModuleId [FilePath]
  deriving (Eq, Show)

data AmbiguousModuleLocationError = AmbiguousModuleLocationError (Maybe BasicAnn) ModuleId [FilePath]
  deriving (Eq, Show)

newtype FileNotFoundError = FileNotFoundError FilePath
  deriving (Eq, Show)

data TextDecodingError = TextDecodingError FilePath Text.Encoding.UnicodeException
  deriving (Eq, Show)

data LexerError = LexerError FilePath (ParseErrorBundle Text Void)
  deriving (Eq, Show)

data ParserError = ParserError FilePath (ParseErrorBundle Lexer.TokenStream Void)
  deriving (Eq, Show)

data DuplicateModuleDeclarationError = DuplicateModuleDeclarationError ModuleId BasicAnn
  deriving (Eq, Show)

type ParseModuleErrors es =
  ( Error TextDecodingError :> es
  , Error LexerError :> es
  , Error ParserError :> es
  )

type ReadModFileErrors es =
  ( Error FileNotFoundError :> es
  , ParseModuleErrors es
  )

readModFile ::
  ( HasCallStack
  , ReadModFileErrors es
  , FileSystem :> es
  ) =>
  FilePath ->
  Eff es (ModDefinitionBody BasicAnn)
readModFile path = do
  exists <- doesFileExist path
  unless exists do
    throwError $ FileNotFoundError path
  bs <- readFile path
  text <- case Text.Encoding.decodeUtf8' bs of
    Left err -> throwError $ TextDecodingError path err
    Right text -> pure text
  tokens <- case Megaparsec.runParser Lexer.tokensWithPos path text of
    Left err -> throwError $ LexerError path err
    Right tokens -> pure tokens
  let tokenStream =
        Lexer.TokenStream
          { tokens = toList tokens
          , inputPos = Megaparsec.initialPos path
          , input = text
          }
  case Megaparsec.runParser pModDefinitionBody path tokenStream of
    Left err -> throwError $ ParserError path err
    Right ast -> pure $ BasicAnn path <$> ast

moduleFilePathCandidates ::
  (HasCallStack) =>
  -- | Absolute path to the package src directory
  FilePath ->
  -- | Module name
  NonEmptyVector Text ->
  [FilePath]
moduleFilePathCandidates rootPath moduleName =
  let rootDir = takeDirectory rootPath

      mod' _ [] = error "unreachable"
      mod' path [lastChunk] = path </> Text.unpack lastChunk </> "mod.fl"
      mod' path (chunk : chunks) = mod' (path </> Text.unpack chunk) chunks

      name _ [] = error "unreachable"
      name path [lastChunk] = path </> (Text.unpack lastChunk <> ".fl")
      name path (chunk : chunks) = name (path </> Text.unpack chunk) chunks
  in  ---
      [ mod' rootDir (toList moduleName)
      , name rootDir (toList moduleName)
      ]

data ModuleDeclaration = ModuleDeclaration
  { visibility :: Visibility
  , identifier :: ModuleId
  , ann :: BasicAnn
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToExpr)

data ModuleDefinition = ModuleDefinition
  { visibility :: Visibility
  , identifier :: ModuleId
  , body :: ModDefinitionBody BasicAnn
  , declAnn :: Maybe BasicAnn
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToExpr)

collectModuleDefinitionModules ::
  forall es.
  (HasCallStack, Error DuplicateModuleDeclarationError :> es) =>
  ModuleDefinition ->
  Eff es (Seq ModuleDefinition, Seq ModuleDeclaration)
collectModuleDefinitionModules modDef = do
  (defs, decls) <- snd <$> collectFromBody modDef.body
  (childDefs, childDecls) <- mconcat . toList <$> for defs collectModuleDefinitionModules
  pure (defs Seq.>< childDefs, decls Seq.>< childDecls)
 where
  collectFromBody body =
    foldM collectFromItem (HashSet.empty, (Seq.empty, Seq.empty)) body.items

  collectFromItem (seen, (defs, decls)) item = case item.item of
    ModItemModF (Mod{mod = ModDeclarationF ident}) -> do
      let moduleId = mkModuleId ident.name
      when (moduleId `HashSet.member` seen) do
        throwError $ DuplicateModuleDeclarationError moduleId ident.ann
      let decl =
            ModuleDeclaration
              { visibility = resolveVisibility modDef.visibility (mPubToVisibility item.pub)
              , identifier = moduleId
              , ann = ident.ann
              }
      pure (HashSet.insert moduleId seen, (defs, decls Seq.|> decl))
    ModItemModF (Mod{mod = ModDefinitionF ident nestedBody}) -> do
      let moduleId = mkModuleId ident.name
      when (moduleId `HashSet.member` seen) do
        throwError $ DuplicateModuleDeclarationError moduleId ident.ann
      let def =
            ModuleDefinition
              { visibility = resolveVisibility modDef.visibility (mPubToVisibility item.pub)
              , identifier = moduleId
              , body = nestedBody
              , declAnn = Just ident.ann
              }
      pure (HashSet.insert moduleId seen, (defs Seq.|> def, decls))
    _ -> pure (seen, (defs, decls))

  mkModuleId ident = ModuleId{package = modDef.identifier.package, path = modDef.identifier.path `Vector.snoc` ident}

  mPubToVisibility mPub = case mPub of
    Nothing -> VisibilityModule modDef.identifier
    Just (PubPackage _) -> VisibilityPackage
    Just PubPub -> VisibilityAll

