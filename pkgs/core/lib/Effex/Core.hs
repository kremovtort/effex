{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Effex.Core where

import "base" Control.Monad (unless, when, foldM)
import "base" Data.Foldable (toList)
import "base" Data.Traversable (for)
import "base" GHC.Stack (HasCallStack)
import "containers" Data.Sequence qualified as Seq
import "effectful" Effectful (Eff, (:>))
import "effectful" Effectful.Error.Static (Error, throwError)
import "effectful" Effectful.Reader.Static (Reader)
import "effectful" Effectful.Reader.Static qualified as Reader
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "text" Data.Text (Text)
import "unordered-containers" Data.HashMap.Strict (HashMap)
import "unordered-containers" Data.HashMap.Strict qualified as HashMap
import "vector" Data.Vector (Vector)
import "vector" Data.Vector qualified as Vector

import Effex.AST.Ann (SourceSpan (..))
import Effex.AST.Surface qualified as AST.Surface
import Effex.AST.Surface.Common qualified as AST.Surface
import Effex.AST.Surface.Module qualified as AST.Surface
import Effex.AST.Surface.Use qualified as AST.Surface
import Effex.Core.Module (ModuleId (..), ModuleEnv (..), PackageId (..))
import Effex.Core.Module qualified as Module

data Module = Module
  { id :: ModuleId
  , submodules :: Vector ModuleId
  }

buildModule ::
  ( HasCallStack
  , Reader GlobalEnv :> es
  , Error UseAnalysisError :> es
  ) =>
  ModuleId ->
  AST.Surface.ModDefinitionBody SourceSpan ->
  Eff es (Module, Vector Module)
buildModule modId ast = do
  _ <- buildEnv
  error "unimplemented"
 where
  buildEnv = do
    uses <- mconcat . toList <$> for ast.uses (unwindUseClause modId)
    foldM addUseToEnv emptyEnv uses
   where
    emptyEnv = ModuleEnv HashMap.empty
    addUseToEnv env = \case
      Use{moduleId = _, use = UsKiVar _ _} -> do
        error "unimplemented"
      Use{moduleId = _, use = UsKiMethod _ _} -> do
        error "unimplemented"
      Use{moduleId = _, use = UsKiType _ _} -> do
        error "unimplemented"
      Use{moduleId = _, use = UsKiMethodAsFn _ _} -> do
        error "unimplemented"
      Use{moduleId = modId', use = UsKiMod} -> do
        pure $ env{Module.modules = HashMap.insert modId' () env.modules}
      Use{moduleId = _, use = UsKiAll} -> do
        error "unimplemented"

data UseKind
  = UsKiVar Text {- as -} (Maybe Text)
  | UsKiMethod Text {- as -} (Maybe Text)
  | UsKiType Text {- as -} (Maybe Text)
  | UsKiMethodAsFn Text {- as fn -} Text
  | UsKiMod
  | UsKiAll
  deriving (Eq, Ord, Show)

data Use = Use
  { moduleId :: ModuleId
  , use :: UseKind
  }

newtype GlobalEnv = GlobalEnv
  { modules :: HashMap ModuleId ()
  }

data UseAnalysisError
  = UsAnErrNoParent SourceSpan
  | UsAnErrModuleNotFound SourceSpan ModuleId
  deriving (Eq, Ord, Show)

unwindUseClause ::
  ( HasCallStack
  , Error UseAnalysisError :> es
  , Reader GlobalEnv :> es
  ) =>
  ModuleId ->
  AST.Surface.UseClause SourceSpan ->
  Eff es (Seq.Seq Use)
unwindUseClause currentModId useClause = do
  useRoot <- case useClause.root of
    AST.Surface.UsClSelf _ -> pure currentModId
    AST.Surface.UsClSupers supers -> do
      let parentDiff = NonEmptyVector.length supers - Vector.length currentModId.path - 1
      when (NonEmptyVector.length supers > Vector.length currentModId.path) do
        throwError $ UsAnErrNoParent (supers NonEmptyVector.! parentDiff)
      pure $
        ModuleId
          { path = Vector.take (Vector.length currentModId.path - parentDiff) currentModId.path
          , package = currentModId.package
          }
    AST.Surface.UsClPackage modId ->
      pure $ ModuleId{path = Vector.empty, package = PackageId modId.name}
  unwindUseTree useRoot useClause.tree
 where
  unwindUseTree !modId = \case
    AST.Surface.UseTrBranch childModId tree -> do
      let childMod = modId{Module.path = modId.path `Vector.snoc` childModId.name}
      checkModuleExists childModId.ann childMod
      unwindUseTree modId{Module.path = modId.path `Vector.snoc` childModId.name} tree
    AST.Surface.UseTrNested trees -> do
      mconcat . Vector.toList <$> for trees (unwindUseTree modId)
    AST.Surface.UseTrLeafWildcard _ ->
      pure $
        Seq.singleton $
          Use{moduleId = modId, use = UsKiAll}
    AST.Surface.UseTrLeafVar leaf -> do
      let modId' = modId{Module.path = modId.path `Vector.snoc` leaf.use.name}
      isModule <- isModuleExists modId'
      if isModule
        then do
          pure $
            Seq.singleton $
              Use
                { moduleId = modId'
                , use = UsKiMod
                }
        else do
          error "unimplemented"
    AST.Surface.UseTrLeafType leaf -> do
      let modId' = modId{Module.path = modId.path `Vector.snoc` leaf.use.name}
      isModule <- isModuleExists modId'
      if isModule
        then do
          pure $
            Seq.singleton $
              Use
                { moduleId = modId'
                , use = UsKiMod
                }
        else do
          error "unimplemented"
    AST.Surface.UseTrLeafMethod _ ->
      error "unimplemented"
    AST.Surface.UseTrLeafMethodAsFn _ ->
      error "unimplemented"

  checkModuleExists ann modId = do
    env <- Reader.ask @GlobalEnv
    unless (HashMap.member modId env.modules) do
      throwError $ UsAnErrModuleNotFound ann modId

  isModuleExists modId = do
    env <- Reader.ask @GlobalEnv
    pure $ HashMap.member modId env.modules

{-
# Разрешение модулей в рамках одного пакета

1. Собрать все модули в пакете в один список, для этого надо распарсить root модуль, найти в нём mod declaration, далее прочитать и распарсить все модули объявленные таким образом. Рекурсивно продолжать этот процесс до тех пор, пока не будут найдены все модули в пакете.
2. Для каждого модуля собрать все объявлённые в них символы, к каждому символу можно прилинковать их AST-ноду
3. Протраверсить все модули на предмет зависимости от других модулей, для этого надо проанализировать все use и квалифицированные доступы к символам, на этом же этапе можно будет проверить, что все символы, которые используются в модуле действительно объявлены. На основе полученной информации можно будет построить граф зависимостей между модулями.
4. Проанализировать полученный граф зависимостей на предмет циклических зависимостей, если таковые есть, то это ошибка компиляции. Если циклических зависимостей нет, то можно построить топологическую сортировку графа, которая будет определять порядок компиляции и возможность параллельного компиляции модулей.
-}
