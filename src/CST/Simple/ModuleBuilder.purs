module CST.Simple.ModuleBuilder
       ( ModuleBuilderT
       , ModuleBuilder
       , buildModule
       , buildModuleT
       , addTypeDecl
       , class AsType
       , asType
       , tyCons
       , tyString
       ) where

import Prelude

import CST.Simple.Internal.Utils (noteM)
import CST.Simple.Names (ModuleName, PName, pname', pnameToProperName, pnameToString, qualNameProper)
import CST.Simple.Types (CodegenError(..), ModuleContent)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (class MonadError, ExceptT, runExceptT)
import Control.Monad.State (class MonadState, StateT, execStateT, gets, modify_)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (for_)
import Data.Identity (Identity)
import Data.List (List, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (uncurry)
import Language.PS.CST (QualifiedName(..))
import Language.PS.CST as CST

type ModuleBuilderState =
  { imports :: Map ModuleName (Set CST.Import)
  , idecls :: List CST.Declaration
  , pnames :: Set PName
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
             , pnames :: Set PName
             } (ModuleBuilderT m)

type ModuleBuilder a = ModuleBuilderT Identity a

buildModule ::
  ModuleBuilder Unit ->
  Either CodegenError ModuleContent
buildModule=
  unwrap <<< buildModuleT

buildModuleT ::
  forall m.
  Monad m =>
  ModuleBuilderT m Unit ->
  m (Either CodegenError ModuleContent)
buildModuleT (ModuleBuilderT mb) = map toContent <$> (runExceptT (execStateT mb mempty))
  where
    toContent ms =
      { imports: uncurry toImportDecl <$> Map.toUnfoldable ms.imports
      , exports: []
      , declarations: Array.fromFoldable $ List.reverse ms.idecls
      }

    toImportDecl moduleName names' =
      CST.ImportDecl { moduleName
                     , names: Set.toUnfoldable names'
                     , qualification: Nothing
                     }

-- Declarations

addTypeDecl :: forall m t. Monad m => AsType t => String -> t -> ModuleBuilderT m Unit
addTypeDecl name t = do
  pname <- mkPName name
  type_ <- asType t
  addDeclaration pname $
    CST.DeclType
    { comments: Nothing
    , head: CST.DataHead
      { dataHdName: pnameToProperName pname
      , dataHdVars: []
      }
    , type_
    }

addImportType :: forall m. Monad m => QualifiedName PName -> ModuleBuilderT m Unit
addImportType (QualifiedName { qualModule, qualName }) = for_ qualModule \qualModule' ->
  modify_ (\s -> s { imports = Map.insertWith append qualModule' (Set.singleton import_) s.imports
                   }
          )
  where
    import_ = CST.ImportType (pnameToProperName qualName) Nothing

addDeclaration :: forall m. Monad m => PName -> CST.Declaration -> ModuleBuilderT m Unit
addDeclaration pname decl = do
  verifyNoDuplicate
  modify_ (\s -> s { idecls = decl : s.idecls
                   , pnames = Set.insert pname s.pnames
                   }
          )

  where
    verifyNoDuplicate = do
      pnames <- gets _.pnames
      if Set.member pname pnames
        then throwError $ DuplicateDeclName $ pnameToString pname
        else pure unit

-- Types

class AsType a where
  asType :: forall m. Monad m => a -> ModuleBuilderT m CST.Type

instance asTypeType :: AsType CST.Type where
  asType = pure

instance asTypeString :: AsType String where
  asType = tyCons

tyCons :: forall m. Monad m => String -> ModuleBuilderT m CST.Type
tyCons t = do
  q@(QualifiedName { qualName }) <- mkQualName t
  addImportType q
  pure $ CST.TypeConstructor $
    QualifiedName { qualModule: Nothing
                  , qualName: pnameToProperName qualName
                  }

tyString :: String -> CST.Type
tyString t = CST.TypeString t

-- Names

mkPName :: forall m. MonadThrow CodegenError m => String -> m PName
mkPName s = noteM (InvalidProperName s) $ pname' s

mkQualName :: forall m. MonadThrow CodegenError m => String -> m (QualifiedName PName)
mkQualName s = noteM (InvalidQualifiedName s) $ qualNameProper s
