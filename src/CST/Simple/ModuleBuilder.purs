module CST.Simple.ModuleBuilder
       ( ModuleBuilderT
       , ModuleBuilder
       , buildModule
       , buildModuleT
       ) where

import Prelude

import CST.Simple.Types (CodegenError, ModuleContent)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (class MonadError, ExceptT)
import Control.Monad.State (StateT)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.Newtype (unwrap)

type ModuleBuilderState =
  {
  }

newtype ModuleBuilderT m a =
  ModuleBuilderT (StateT ModuleBuilderState (ExceptT CodegenError m) a)

derive newtype instance moduleBuilderTFunctor :: Functor m => Functor (ModuleBuilderT m)
derive newtype instance moduleBuilderTApply :: Monad m => Apply (ModuleBuilderT m)
derive newtype instance moduleBuilderTApplicative :: Monad m => Applicative (ModuleBuilderT m)
derive newtype instance moduleBuilderTBind :: Monad m => Bind (ModuleBuilderT m)
derive newtype instance moduleBuilderTMonad :: Monad m => Monad (ModuleBuilderT m)
derive newtype instance moduleBuilderTMonadThrow :: Monad m => MonadThrow CodegenError (ModuleBuilderT m)
derive newtype instance moduleBuilderTMonadError :: Monad m => MonadError CodegenError (ModuleBuilderT m)

type ModuleBuilder a = ModuleBuilderT Identity a

buildModule ::
  String ->
  ModuleBuilder Unit ->
  Either CodegenError ModuleContent
buildModule mname mb =
  unwrap $ buildModuleT mname mb

buildModuleT ::
  forall m.
  Monad m =>
  String ->
  ModuleBuilderT m Unit ->
  m (Either CodegenError ModuleContent)
buildModuleT mname mb =
  pure (pure { declarations: []
             , exports: []
             , imports: []
             }
       )
