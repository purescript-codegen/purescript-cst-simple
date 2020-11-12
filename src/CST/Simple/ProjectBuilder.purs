module CST.Simple.ProjectBuilder
       ( ProjectBuilderT
       , ProjectBuilder
       , buildProject
       , buildProjectT
       , addModule
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, buildModule)
import CST.Simple.Internal.Utils (exceptM)
import CST.Simple.Names (ModuleName, readName')
import CST.Simple.Types (Project, ModuleEntry)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (class MonadState, StateT(..), execStateT)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Language.PS.CST as CST

type BuilderState =
  { moduleMap :: Map ModuleName ModuleEntry
  }

newtype ProjectBuilderT m a =
  ProjectBuilderT (StateT BuilderState (ExceptT CodegenError m) a)

derive newtype instance projectBuilderTFunctor :: Functor m => Functor (ProjectBuilderT m)
derive newtype instance projectBuilderTBind :: Monad m => Bind (ProjectBuilderT m)
derive newtype instance projectBuilderTApply :: Monad m => Apply (ProjectBuilderT m)
derive newtype instance projectBuilderTApplicative :: Monad m => Applicative (ProjectBuilderT m)
derive newtype instance projectBuilderTMonad :: Monad m => Monad (ProjectBuilderT m)
derive newtype instance projectBuilderTMonadThrow :: Monad m => MonadThrow CodegenError (ProjectBuilderT m)
derive newtype instance projectBuilderTMonadError :: Monad m => MonadError CodegenError (ProjectBuilderT m)
derive newtype instance projectBuilderTMonadState ::
  Monad m => MonadState
  { moduleMap :: Map ModuleName { cstModule :: CST.Module
                                , foreignBinding :: Maybe String
                                }
  } (ProjectBuilderT m)

type ProjectBuilder a = ProjectBuilderT Identity a

buildProjectT ::
  forall m.
  Functor m =>
  ProjectBuilderT m Unit ->
  m (Either CodegenError Project)
buildProjectT (ProjectBuilderT pb) =
  map mkProject <$> (runExceptT $ execStateT pb mempty)
  where
    mkProject { moduleMap } =
      { modules: List.toUnfoldable $ Map.values moduleMap
      }

buildProject :: ProjectBuilder Unit -> (Either CodegenError Project)
buildProject = unwrap <<< buildProjectT

addModule :: String -> ModuleBuilder Unit -> ProjectBuilder Unit
addModule name mb = do
  me <- exceptM $ buildModule name mb
  moduleName <- readName' name
  ProjectBuilderT $ StateT \s ->
    case Map.lookup moduleName s.moduleMap of
      Just _ ->
        throwError (DuplicateModuleName name)
      Nothing -> do
        pure $ unit /\
          s { moduleMap = Map.insert moduleName me s.moduleMap
            }
