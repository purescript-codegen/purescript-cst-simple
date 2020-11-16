module CST.Simple.Internal.ModuleBuilder
       ( ModuleBuilder
       , buildModule
       , buildModule'
       , addImport
       , exportAll
       , addCSTExport
       , addCSTDeclaration
       , addForeignBinding
       , mkName
       , mkQualName
       , mkQualConstructorName
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Internal.Import (class AsImport, asImport)
import CST.Simple.Internal.Utils (exceptM)
import CST.Simple.Names (class ReadName, class UnwrapQualName, ConstructorName, ModuleName, QualifiedName, TypedConstructorName(..), qualName, readName')
import CST.Simple.Types (ModuleEntry)
import Control.Alt (class Alt, (<|>))
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except.Trans (class MonadThrow)
import Control.Monad.State (StateT, evalStateT, get)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Trans (modify_)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (fold, for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Language.PS.CST as CST

type ModuleBuilderState =
  { imports :: Map ModuleName (Set CST.Import)
  , exports :: Exports
  , decls :: Array CST.Declaration
  , foreignBinding :: Maybe String
  }

newtype ModuleBuilder a =
  ModuleBuilder (StateT ModuleBuilderState (Either CodegenError) a)

derive newtype instance moduleBuilderFunctor :: Functor ModuleBuilder
derive newtype instance moduleBuilderApply :: Apply ModuleBuilder
derive newtype instance moduleBuilderApplicative :: Applicative ModuleBuilder
derive newtype instance moduleBuilderBind :: Bind ModuleBuilder
derive newtype instance moduleBuilderMonad :: Monad ModuleBuilder
derive newtype instance moduleBuilderMonadThrow :: MonadThrow CodegenError ModuleBuilder
derive newtype instance moduleBuilderMonadError :: MonadError CodegenError ModuleBuilder
derive newtype instance moduleBuilderMonadState ::
  MonadState { imports :: Map ModuleName (Set CST.Import)
             , exports :: Exports
             , decls :: Array CST.Declaration
             , foreignBinding :: Maybe String
             } ModuleBuilder
derive newtype instance moduleBuilderAlt :: Alt ModuleBuilder

data Exports =
  ExportAll
  | ExportSelected (Array CST.Export)

instance exportsSemigroup :: Semigroup Exports where
  append ExportAll _ = ExportAll
  append _ ExportAll = ExportAll
  append (ExportSelected s1) (ExportSelected s2) = ExportSelected (s1 <> s2)

instance exportsMonoid :: Monoid Exports where
  mempty = ExportSelected []

buildModule ::
  String ->
  ModuleBuilder Unit ->
  Either CodegenError ModuleEntry
buildModule moduleName' mb =
  snd <$> buildModule' moduleName' mb

buildModule' ::
  forall a.
  String ->
  ModuleBuilder a ->
  Either CodegenError (a /\ ModuleEntry)
buildModule' moduleName' mb =
  evalStateT mb' mempty
  where
    getExports es = case es of
      ExportSelected [] -> throwError MissingExports
      ExportSelected es' -> pure $ es'
      ExportAll -> pure []

    toImportDecl moduleName names' =
      CST.ImportDecl { moduleName
                     , names: Set.toUnfoldable names'
                     , qualification: Nothing
                     }

    (ModuleBuilder mb') = do
      moduleName <- readName' moduleName'
      a <- mb
      c <- get
      exports <- getExports c.exports
      pure $ a
        /\ { cstModule: CST.Module
             { moduleName
             , imports: uncurry toImportDecl <$> Map.toUnfoldable c.imports
             , exports
             , declarations: c.decls
             }
           , foreignBinding: c.foreignBinding
           }


addImport :: ModuleName -> CST.Import -> ModuleBuilder Unit
addImport moduleName import_ =
  modify_ (\s -> s { imports = Map.insertWith append moduleName (Set.singleton import_) s.imports
                   }
          )

exportAll :: ModuleBuilder Unit
exportAll =
  modify_ (_ { exports = ExportAll
             }
          )

addCSTExport :: CST.Export -> ModuleBuilder Unit
addCSTExport export =
  modify_ (\s -> s { exports = case s.exports of
                        ExportAll -> ExportAll
                        ExportSelected es -> ExportSelected (Array.snoc es export)
                   }
          )


addCSTDeclaration :: CST.Declaration -> ModuleBuilder Unit
addCSTDeclaration decl = do
  modify_ (\s -> s { decls = Array.snoc s.decls decl
                   }
          )

addForeignBinding :: String -> ModuleBuilder Unit
addForeignBinding b =
  modify_ (\s -> s { foreignBinding = Just $ fold s.foreignBinding <> b
                   }
          )

-- Names

mkName ::
  forall n.
  ReadName n =>
  String ->
  ModuleBuilder n
mkName = readName'

mkQualName ::
  forall n.
  AsImport n =>
  UnwrapQualName n =>
  ReadName n =>
  String ->
  ModuleBuilder (QualifiedName n)
mkQualName s = do
  CST.QualifiedName q <- exceptM $ qualName s
  for_ q.qualModule \m ->
    addImport m (asImport q.qualName)
  pure $ CST.QualifiedName (q { qualModule = Nothing })

mkQualConstructorName ::
  String ->
  ModuleBuilder (QualifiedName ConstructorName)
mkQualConstructorName c = qualifiedCons <|> unqualifiedCons
  where
    qualifiedCons =
      map getNamePart <$> mkQualName c

    unqualifiedCons = mkName c <#> \name ->
      CST.QualifiedName { qualModule: Nothing
                        , qualName: name
                        }

    getNamePart (TypedConstructorName _ n) = n
