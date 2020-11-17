module CST.Simple.ProjectBuilder
       ( ProjectBuilderT
       , ProjectBuilder
       , ProjectError(..)
       , buildProject
       , buildProjectT
       , addModule
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError)
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, buildModule)
import CST.Simple.Internal.Utils (exceptM)
import CST.Simple.Names (ModuleName)
import CST.Simple.Types (Project, ModuleEntry)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (class MonadState, StateT(..), execStateT)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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

data ProjectError =
  ProjectModuleError String CodegenError
  | DuplicateModuleName String

derive instance projectErrorEq :: Eq ProjectError
derive instance projectErrorOrd :: Ord ProjectError
derive instance projectErrorGeneric :: Generic ProjectError _
instance projectErrorShow :: Show ProjectError where
  show x = genericShow x

newtype ProjectBuilderT m a =
  ProjectBuilderT (StateT BuilderState (ExceptT ProjectError m) a)

derive newtype instance projectBuilderTFunctor :: Functor m => Functor (ProjectBuilderT m)
derive newtype instance projectBuilderTBind :: Monad m => Bind (ProjectBuilderT m)
derive newtype instance projectBuilderTApply :: Monad m => Apply (ProjectBuilderT m)
derive newtype instance projectBuilderTApplicative :: Monad m => Applicative (ProjectBuilderT m)
derive newtype instance projectBuilderTMonad :: Monad m => Monad (ProjectBuilderT m)
derive newtype instance projectBuilderTMonadThrow :: Monad m => MonadThrow ProjectError (ProjectBuilderT m)
derive newtype instance projectBuilderTMonadError :: Monad m => MonadError ProjectError (ProjectBuilderT m)
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
  m (Either ProjectError Project)
buildProjectT (ProjectBuilderT pb) =
  map mkProject <$> (runExceptT $ execStateT pb mempty)
  where
    mkProject { moduleMap } =
      { modules: List.toUnfoldable $ Map.values moduleMap
      }

buildProject :: ProjectBuilder Unit -> Either ProjectError Project
buildProject = unwrap <<< buildProjectT

addModule :: String -> ModuleBuilder Unit -> ProjectBuilder Unit
addModule name mb = do
  me <- buildModule'
  let moduleName = getModuleName me
  ProjectBuilderT $ StateT \s ->
    case Map.lookup moduleName s.moduleMap of
      Just _ ->
        throwError (DuplicateModuleName name)
      Nothing -> do
        pure $ unit /\
          s { moduleMap = Map.insert moduleName me s.moduleMap
            }
  where
    buildModule' = exceptM $ (lmap (ProjectModuleError name)) (buildModule name mb)
    getModuleName { cstModule: CST.Module { moduleName } } = moduleName
