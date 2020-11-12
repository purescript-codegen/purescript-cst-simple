module CST.Simple.Internal.ModuleBuilder
       ( ModuleBuilderT
       , ModuleBuilder
       , execModuleBuilder
       , execModuleBuilderT
       , runModuleBuilder
       , runModuleBuilderT
       , liftModuleBuilder
       , addImport
       , addCSTDeclaration
       , addForeignBinding
       , mkName
       , mkQualName
       , mkQualConstructorName
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError)
import CST.Simple.Internal.Import (class AsImport, asImport)
import CST.Simple.Internal.Utils (exceptM)
import CST.Simple.Names (class ReadName, class UnwrapQualName, ConstructorName, ModuleName, QualifiedName, TypedConstructorName(..), qualName, readName')
import CST.Simple.Types (ModuleContent)
import Control.Alt (class Alt, (<|>))
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except (ExceptT, mapExceptT, runExceptT)
import Control.Monad.Except.Trans (class MonadThrow)
import Control.Monad.State (StateT, mapStateT, runStateT)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Trans (modify_)
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.Foldable (fold, for_)
import Data.Identity (Identity)
import Data.List (List, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested (type (/\))
import Language.PS.CST as CST

type ModuleBuilderState =
  { imports :: Map ModuleName (Set CST.Import)
  , idecls :: List CST.Declaration
  , pnames :: Set String
  , foreignBinding :: Maybe String
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
derive newtype instance moduleBuilderTMonadState :: Monad m =>
  MonadState { imports :: Map ModuleName (Set CST.Import)
             , idecls :: List CST.Declaration
             , pnames :: Set String
             , foreignBinding :: Maybe String
             } (ModuleBuilderT m)
derive newtype instance moduleBuilderTAlt :: Monad m => Alt (ModuleBuilderT m)

type ModuleBuilder a = ModuleBuilderT Identity a

execModuleBuilder ::
  ModuleBuilder Unit ->
  Either CodegenError ModuleContent
execModuleBuilder =
  unwrap <<< execModuleBuilderT

execModuleBuilderT ::
  forall m.
  Monad m =>
  ModuleBuilderT m Unit ->
  m (Either CodegenError ModuleContent)
execModuleBuilderT = map (map snd) <<< runModuleBuilderT

runModuleBuilder ::
  forall a.
  ModuleBuilder a ->
  Either CodegenError (a /\ ModuleContent)
runModuleBuilder =
  unwrap <<< runModuleBuilderT

runModuleBuilderT ::
  forall m a.
  Monad m =>
  ModuleBuilderT m a ->
  m (Either CodegenError (a /\ ModuleContent))
runModuleBuilderT (ModuleBuilderT mb) = map (rmap toContent) <$> (runExceptT (runStateT mb mempty))
  where
    toContent ms =
      { imports: uncurry toImportDecl <$> Map.toUnfoldable ms.imports
      , exports: []
      , declarations: Array.fromFoldable $ List.reverse ms.idecls
      , foreignBinding: ms.foreignBinding
      }

    toImportDecl moduleName names' =
      CST.ImportDecl { moduleName
                     , names: Set.toUnfoldable names'
                     , qualification: Nothing
                     }


addImport :: forall m. Monad m => ModuleName -> CST.Import -> ModuleBuilderT m Unit
addImport moduleName import_ =
  modify_ (\s -> s { imports = Map.insertWith append moduleName (Set.singleton import_) s.imports
                   }
          )

addCSTDeclaration :: forall m. Monad m => CST.Declaration -> ModuleBuilderT m Unit
addCSTDeclaration decl = do
  modify_ (\s -> s { idecls = decl : s.idecls
                   }
          )

addForeignBinding :: forall m. Monad m => String -> ModuleBuilderT m Unit
addForeignBinding b =
  modify_ (\s -> s { foreignBinding = Just $ fold s.foreignBinding <> b
                   }
          )

liftModuleBuilder :: forall m a. Monad m => ModuleBuilder a -> ModuleBuilderT m a
liftModuleBuilder (ModuleBuilderT x) =
  (ModuleBuilderT $ mapStateT (mapExceptT (pure <<< unwrap)) x)

-- Names

mkName ::
  forall m n.
  Monad m =>
  ReadName n =>
  String ->
  ModuleBuilderT m n
mkName = readName'

mkQualName ::
  forall m n.
  Monad m =>
  AsImport n =>
  UnwrapQualName n =>
  ReadName n =>
  String ->
  ModuleBuilderT m (QualifiedName n)
mkQualName s = do
  CST.QualifiedName q <- exceptM $ qualName s
  for_ q.qualModule \m ->
    addImport m (asImport q.qualName)
  pure $ CST.QualifiedName (q { qualModule = Nothing })

mkQualConstructorName ::
  forall m.
  Monad m =>
  String ->
  ModuleBuilderT m (QualifiedName ConstructorName)
mkQualConstructorName c = qualifiedCons <|> unqualifiedCons
  where
    qualifiedCons =
      map getNamePart <$> mkQualName c

    unqualifiedCons = mkName c <#> \name ->
      CST.QualifiedName { qualModule: Nothing
                        , qualName: name
                        }

    getNamePart (TypedConstructorName _ n) = n
