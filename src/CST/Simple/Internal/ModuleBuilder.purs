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
       , mkProperName
       , mkIdent
       , mkQualName
       , mkQualProperName
       , mkQualIdent
       , mkQualOpName
       ) where

import Prelude

import CST.Simple.Internal.Utils (noteM)
import CST.Simple.Names (Ident, ModuleName, OpName, ProperName, QualifiedName(..), ident', properName', qualNameIdent, qualNameOp, qualNameProper)
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
  , pnames :: Set String
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


addImportType :: forall m. Monad m => QualifiedName (ProperName CST.ProperNameType_TypeName) -> ModuleBuilderT m Unit
addImportType = addQualImport \n -> CST.ImportType n Nothing

addImportKind :: forall m. Monad m => QualifiedName (ProperName CST.ProperNameType_KindName) -> ModuleBuilderT m Unit
addImportKind = addQualImport CST.ImportKind

addImportClass :: forall m. Monad m => QualifiedName (ProperName CST.ProperNameType_ClassName) -> ModuleBuilderT m Unit
addImportClass = addQualImport CST.ImportClass

addQualImport ::
  forall p m.
  Monad m =>
  (ProperName p -> CST.Import) ->
  QualifiedName (CST.ProperName p) ->
  ModuleBuilderT m Unit
addQualImport toCSTImport (QualifiedName { qualModule, qualName }) =
  for_ qualModule \m ->
  addImport m (toCSTImport qualName)

addImport :: forall m. Monad m => ModuleName -> CST.Import -> ModuleBuilderT m Unit
addImport moduleName import_ =
  modify_ (\s -> s { imports = Map.insertWith append moduleName (Set.singleton import_) s.imports
                   }
          )

addDeclaration :: forall m. Monad m => String -> CST.Declaration -> ModuleBuilderT m Unit
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
        then throwError $ DuplicateDeclName $ pname
        else pure unit

liftModuleBuilder :: forall m a. Monad m => ModuleBuilder a -> ModuleBuilderT m a
liftModuleBuilder (ModuleBuilderT x) =
  (ModuleBuilderT $ mapStateT (mapExceptT (pure <<< unwrap)) x)

-- Names

mkProperName :: forall m p. MonadThrow CodegenError m => String -> m (ProperName p)
mkProperName s = noteM (InvalidProperName s) $ properName' s

mkIdent :: forall m. MonadThrow CodegenError m => String -> m Ident
mkIdent s = noteM (InvalidIdent s) $ ident' s

mkQualName ::
  forall m n.
  MonadThrow CodegenError m =>
  (String -> Maybe (QualifiedName n)) ->
  String ->
  m (QualifiedName n)
mkQualName f s =
  noteM (InvalidQualifiedName s) $ f s

mkQualProperName ::
  forall m p.
  MonadThrow CodegenError m =>
  String ->
  m (QualifiedName (ProperName p))
mkQualProperName =
  mkQualName qualNameProper

mkQualIdent ::
  forall m.
  MonadThrow CodegenError m =>
  String ->
  m (QualifiedName Ident)
mkQualIdent =
  mkQualName qualNameIdent

mkQualOpName ::
  forall m p.
  MonadThrow CodegenError m =>
  String ->
  m (QualifiedName (OpName p))
mkQualOpName =
  mkQualName qualNameOp
