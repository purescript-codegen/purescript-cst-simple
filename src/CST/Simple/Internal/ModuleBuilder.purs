module CST.Simple.Internal.ModuleBuilder
       ( ModuleBuilderT
       , ModuleBuilder
       , buildModule
       , buildModuleT
       , buildModule'
       , buildModuleT'
       , liftModuleBuilder
       , addImportType
       , addImportKind
       , addImportClass
       , addImport
       , addDeclaration
       , mkPName
       , mkIName
       , mkQualName
       , mkQualPName
       , mkQualCSTIdent
       , mkQualOpName
       ) where

import Prelude

import CST.Simple.Internal.Utils (noteM)
import CST.Simple.Names (IName, ModuleName, OpName, PName, QualifiedName(..), iname', inameToIdent, pname', pnameToProperName, pnameToString, qualNameIdent, qualNameOp, qualNameProper)
import CST.Simple.Types (CodegenError(..), ModuleContent)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (ExceptT, mapExceptT, runExceptT)
import Control.Monad.Except.Trans (class MonadThrow)
import Control.Monad.State (StateT, gets, mapStateT, runStateT)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Trans (modify_)
import Data.Array as Array
import Data.Bifunctor (rmap)
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
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested (type (/\))
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
buildModule =
  unwrap <<< buildModuleT

buildModuleT ::
  forall m.
  Monad m =>
  ModuleBuilderT m Unit ->
  m (Either CodegenError ModuleContent)
buildModuleT = map (map snd) <<< buildModuleT'

buildModule' ::
  forall a.
  ModuleBuilder a ->
  Either CodegenError (a /\ ModuleContent)
buildModule' =
  unwrap <<< buildModuleT'

buildModuleT' ::
  forall m a.
  Monad m =>
  ModuleBuilderT m a ->
  m (Either CodegenError (a /\ ModuleContent))
buildModuleT' (ModuleBuilderT mb) = map (rmap toContent) <$> (runExceptT (runStateT mb mempty))
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


addImportType :: forall m. Monad m => QualifiedName PName -> ModuleBuilderT m Unit
addImportType = addQualImport \n -> CST.ImportType n Nothing

addImportKind :: forall m. Monad m => QualifiedName PName -> ModuleBuilderT m Unit
addImportKind = addQualImport CST.ImportKind

addImportClass :: forall m. Monad m => QualifiedName PName -> ModuleBuilderT m Unit
addImportClass = addQualImport CST.ImportClass

addQualImport ::
  forall p m.
  Monad m =>
  (CST.ProperName p -> CST.Import) ->
  QualifiedName PName ->
  ModuleBuilderT m Unit
addQualImport toCSTImport (QualifiedName { qualModule, qualName }) =
  for_ qualModule (\m -> addImport m import_)

  where
    import_ = toCSTImport (pnameToProperName qualName)

addImport :: forall m. Monad m => ModuleName -> CST.Import -> ModuleBuilderT m Unit
addImport moduleName import_ =
  modify_ (\s -> s { imports = Map.insertWith append moduleName (Set.singleton import_) s.imports
                   }
          )

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

liftModuleBuilder :: forall m a. Monad m => ModuleBuilder a -> ModuleBuilderT m a
liftModuleBuilder (ModuleBuilderT x) =
  (ModuleBuilderT $ mapStateT (mapExceptT (pure <<< unwrap)) x)

-- Names

mkPName :: forall m. MonadThrow CodegenError m => String -> m PName
mkPName s = noteM (InvalidProperName s) $ pname' s

mkIName :: forall m. MonadThrow CodegenError m => String -> m IName
mkIName s = noteM (InvalidIdent s) $ iname' s

mkQualName ::
  forall m n.
  MonadThrow CodegenError m =>
  (String -> Maybe (QualifiedName n)) ->
  String ->
  m (QualifiedName n)
mkQualName f s =
  noteM (InvalidQualifiedName s) $ f s

mkQualPName ::
  forall m.
  MonadThrow CodegenError m =>
  String ->
  m (QualifiedName PName)
mkQualPName =
  mkQualName qualNameProper

mkQualCSTIdent ::
  forall m.
  MonadThrow CodegenError m =>
  String ->
  m (QualifiedName CST.Ident)
mkQualCSTIdent =
  mkQualName (map (map inameToIdent) <<< qualNameIdent)

mkQualOpName ::
  forall m.
  MonadThrow CodegenError m =>
  String ->
  m (QualifiedName OpName)
mkQualOpName =
  mkQualName qualNameOp
