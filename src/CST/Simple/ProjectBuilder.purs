module CST.Simple.ProjectBuilder
       ( Project
       , ProjectBuilderT
       , ProjectBuilder
       , buildProject
       , buildProjectT
       , addModule
       , getCSTModules
       ) where

import Prelude

import CST.Simple.Internal.Utils (noteM)
import CST.Simple.ModuleBuilder (ModuleBuilder)
import CST.Simple.Names (moduleName')
import CST.Simple.Types (CodegenError(..))
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (class MonadState, StateT, execStateT, state)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Language.PS.CST (Module(..), ModuleName)

newtype Project =
  Project { modules :: Array Module
          }

instance projectShow :: Show Project where
  show (Project p) = "(Project " <> show r <> ")"
    where
      r = { modules: showModule <$> p.modules
          }

      showModule (Module m) = "(Module " <> show m.moduleName <> ")"

type BuilderState =
  { moduleMap :: Map String Module
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
  Monad m => MonadState { moduleMap :: Map String Module } (ProjectBuilderT m)

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
      Project { modules: List.toUnfoldable $ Map.values moduleMap
              }

buildProject :: ProjectBuilder Unit -> (Either CodegenError Project)
buildProject = unwrap <<< buildProjectT

addModule :: String -> ModuleBuilder Unit -> ProjectBuilder Unit
addModule name _ = do
  moduleName <- mkModuleName name
  join $ state \s ->
    case Map.lookup name s.moduleMap of
      Just _ ->
        throwError (DuplicateModuleName name) /\ s
      Nothing ->
        pure unit /\
        s { moduleMap = Map.insert name (emptyModule moduleName) s.moduleMap
          }
  where
    emptyModule moduleName =
      Module { moduleName
             , imports: []
             , exports: []
             , declarations: []
             }

getCSTModules :: Project -> Array Module
getCSTModules (Project { modules }) = modules

-- Move to utils

mkModuleName :: forall m. MonadThrow CodegenError m => String -> m ModuleName
mkModuleName s = noteM (InvalidModuleName s) $ moduleName' s
