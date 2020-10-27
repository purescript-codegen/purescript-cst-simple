module CST.Simple.ModuleBuilder
       ( ModuleBuilderT
       , ModuleBuilder
       , buildModule
       , buildModuleT
       , addTypeDecl
       , Typ
       , runTyp
       , typVar
       , typCons
       , typString
       , typRow
       , typRow_
       , typRecord
       , typRecord_
       , typApp
       , typForall
       , typArrow
       , (*->)
       , typKinded
       , (*::)
       , typOp
       , typConstrained
       , (*=>)
       , class AsTyp
       , asTyp
       , Constraint
       , cnst
       ) where

import Prelude

import CST.Simple.Internal.Utils (noteM)
import CST.Simple.Names (IName, ModuleName, OpName, PName, iname', inameToIdent, opNameToOpName, pname', pnameToProperName, pnameToString, qualNameOp, qualNameProper)
import CST.Simple.Types (CodegenError(..), ModuleContent)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (class MonadError, ExceptT, mapExceptT, runExceptT)
import Control.Monad.State (class MonadState, StateT, execStateT, gets, mapStateT, modify_)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either)
import Data.Foldable (foldl, for_)
import Data.Identity (Identity)
import Data.List (List, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
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
buildModule =
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

addTypeDecl :: forall m t. Monad m => AsTyp t => String -> t -> ModuleBuilderT m Unit
addTypeDecl name t = do
  pname <- mkPName name
  type_ <- runTyp' t
  addDeclaration pname $
    CST.DeclType
    { comments: Nothing
    , head: CST.DataHead
      { dataHdName: pnameToProperName pname
      , dataHdVars: []
      }
    , type_
    }

-- Private Builders

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

-- Typ

newtype Typ = Typ (ModuleBuilder CST.Type)

runTyp :: forall m. Monad m => Typ -> ModuleBuilderT m CST.Type
runTyp (Typ mb) = liftModuleBuilder mb

typVar :: String -> Typ
typVar s = Typ $ CST.TypeVar <<< inameToIdent <$> mkIName s

typCons :: String -> Typ
typCons s = Typ do
  q@(QualifiedName { qualName }) <- mkQualPName s
  addImportType q
  pure $ CST.TypeConstructor $
    QualifiedName { qualModule: Nothing
                  , qualName: pnameToProperName qualName
                  }

typString :: String -> Typ
typString = Typ <<< pure <<< CST.TypeString

typRow :: Array (String /\ Typ) -> Maybe String -> Typ
typRow = typLabelled CST.TypeRow

typRow_ :: Array (String /\ Typ) -> Typ
typRow_ pairs = typRow pairs Nothing

typRecord :: Array (String /\ Typ) -> Maybe String -> Typ
typRecord = typLabelled CST.TypeRecord

typRecord_ :: Array (String /\ Typ) -> Typ
typRecord_ pairs = typRow pairs Nothing

typLabelled ::
  ( { rowLabels :: Array { label :: CST.Label, type_ :: CST.Type}
    , rowTail :: Maybe CST.Type
    } -> CST.Type
  ) ->
  Array (String /\ Typ) ->
  Maybe String ->
  Typ
typLabelled f pairs tailName = Typ ado
  rowLabels <- traverse (uncurry toRowLabel) pairs
  rowTail <- traverse toRowTail tailName
  in f { rowLabels, rowTail }

  where
    toRowLabel l typ = runTyp typ <#> \type_ ->
      { label: CST.Label l, type_ }

    toRowTail s = runTyp (typVar s)

typApp :: forall c a. AsTyp c => AsTyp a => c -> Array a -> Typ
typApp c as = Typ $ foldl f (runTyp (asTyp c)) as
  where
    f acc' a' = CST.TypeApp
      <$> acc'
      <*> runTyp' a'

typForall :: forall t. AsTyp t => Array String -> t -> Typ
typForall vs t = case NonEmptyArray.fromArray vs of
  Just vs' ->
    Typ $
    CST.TypeForall <$> traverse toTypeVarName vs' <*> runTyp t'
  Nothing ->
    t'

  where
    t' = asTyp t

    toTypeVarName v = CST.TypeVarName <<< inameToIdent <$> mkIName v

typArrow :: forall t1 t2. AsTyp t1 => AsTyp t2 => t1 -> t2 -> Typ
typArrow t1 t2 = Typ $ CST.TypeArr <$> runTyp' t1 <*> runTyp' t2

infixr 6 typArrow as *->

typKinded :: forall t. AsTyp t => t -> String -> Typ
typKinded t k = Typ do
  t' <- runTyp' t
  q@(QualifiedName { qualName }) <- mkQualPName k
  addImportKind q
  pure $ CST.TypeKinded t' $ CST.KindName $
    QualifiedName { qualModule: Nothing
                  , qualName: pnameToProperName qualName
                  }

infixr 8 typKinded as *::

typOp :: forall t1 t2. AsTyp t1 => AsTyp t2 => t1 -> String -> t2 -> Typ
typOp t1 op t2 = Typ do
  t1' <- runTyp' t1
  op'@(QualifiedName { qualModule, qualName }) <- map opNameToOpName <$> mkQualOpName op
  t2' <- runTyp' t2

  for_ qualModule \qm ->
    addImport qm $ CST.ImportTypeOp qualName
  let op'' = QualifiedName { qualModule: Nothing, qualName }

  pure $ CST.TypeOp t1' op'' t2'

typConstrained :: forall t. AsTyp t => Constraint -> t -> Typ
typConstrained c t = Typ $ CST.TypeConstrained
  <$> runConstraint c
  <*> runTyp' t

infixr 10 typConstrained as *=>

-- AsTyp

class AsTyp a where
  asTyp :: a -> Typ

instance asTypTyp :: AsTyp Typ where
  asTyp = identity

instance asTypString :: AsTyp String where
  asTyp = typCons

runTyp' :: forall m a. Monad m => AsTyp a => a -> ModuleBuilderT m CST.Type
runTyp' = runTyp <<< asTyp

-- Constraint

newtype Constraint = SConstraint (ModuleBuilder CST.Constraint)

runConstraint :: forall m. Monad m => Constraint -> ModuleBuilderT m CST.Constraint
runConstraint (SConstraint mb) = liftModuleBuilder mb

cnst :: forall t. AsTyp t => String -> Array t -> Constraint
cnst s args = SConstraint do
  q@(QualifiedName { qualName }) <- mkQualPName s
  addImportClass q
  args' <- traverse runTyp' args
  pure $ CST.Constraint
    { className: QualifiedName { qualModule: Nothing
                               , qualName: pnameToProperName qualName
                               }
    , args: args'
    }

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

mkQualOpName ::
  forall m.
  MonadThrow CodegenError m =>
  String ->
  m (QualifiedName OpName)
mkQualOpName =
  mkQualName qualNameOp

-- Utils

liftModuleBuilder :: forall m a. Monad m => ModuleBuilder a -> ModuleBuilderT m a
liftModuleBuilder (ModuleBuilderT x) =
  (ModuleBuilderT $ mapStateT (mapExceptT (pure <<< unwrap)) x)
