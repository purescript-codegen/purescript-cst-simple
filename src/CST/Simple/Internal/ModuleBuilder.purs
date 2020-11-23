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
import CST.Simple.Names (class AsNameError, class ReadName, class UnwrapQualName, ConstructorName, ModuleName, QualifiedName, TypedConstructorName(..), qualName, readName')
import CST.Simple.Types (ModuleEntry)
import Control.Alt (class Alt, (<|>))
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Except.Trans (class MonadThrow)
import Control.Monad.Writer (class MonadTell, class MonadWriter, Writer, runWriter, tell)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either)
import Data.Foldable (fold, foldr, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Language.PS.CST as CST

type ModuleBuilderState =
  { imports :: Array (ModuleName /\ CST.Import)
  , exports :: Exports
  , decls :: Array CST.Declaration
  , foreignBinding :: Maybe String
  }

newtype ModuleBuilder a =
  ModuleBuilder (ExceptT CodegenError (Writer ModuleBuilderState) a)

derive newtype instance moduleBuilderFunctor :: Functor ModuleBuilder
derive newtype instance moduleBuilderApply :: Apply ModuleBuilder
derive newtype instance moduleBuilderApplicative :: Applicative ModuleBuilder
derive newtype instance moduleBuilderBind :: Bind ModuleBuilder
derive newtype instance moduleBuilderMonad :: Monad ModuleBuilder
derive newtype instance moduleBuilderMonadThrow :: MonadThrow CodegenError ModuleBuilder
derive newtype instance moduleBuilderMonadError :: MonadError CodegenError ModuleBuilder
derive newtype instance moduleBuilderMonadTell ::
  MonadTell
  { imports :: Array (ModuleName /\ CST.Import)
  , exports :: Exports
  , decls :: Array CST.Declaration
  , foreignBinding :: Maybe String
  } ModuleBuilder
derive newtype instance moduleBuilderMonadWriter ::
  MonadWriter
  { imports :: Array (ModuleName /\ CST.Import)
  , exports :: Exports
  , decls :: Array CST.Declaration
  , foreignBinding :: Maybe String
  } ModuleBuilder
derive newtype instance moduleBuilderAlt :: Alt ModuleBuilder

instance moduleBuilderEq :: Eq a => Eq (ModuleBuilder a) where
  eq m1 m2 =
    runModuleBuilder m1 == runModuleBuilder m2

instance moduleBuilderOrd :: Ord a => Ord (ModuleBuilder a) where
  compare m1 m2 =
    compare (runModuleBuilder m1) (runModuleBuilder m2)

instance moduleBuilderShow :: Show a => Show (ModuleBuilder a) where
  show m =
    "(ModuleBuilder " <> show m <> ")"

data Exports =
  ExportAll
  | ExportSelected (Array CST.Export)

derive instance exportsEq :: Eq Exports
derive instance exportsOrd :: Ord Exports

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
buildModule' moduleName' mb = do
  moduleName <- readName' moduleName'
  let a' /\ c = runModuleBuilder mb
  a <- a'
  exports <- getExports c.exports
  pure $ a
    /\ { cstModule: CST.Module
         { moduleName
         , imports: buildImportDecls c.imports
         , exports
         , declarations: c.decls
         }
       , foreignBinding: c.foreignBinding
       }

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

runModuleBuilder :: forall a. ModuleBuilder a -> Either CodegenError a /\ ModuleBuilderState
runModuleBuilder (ModuleBuilder mb) =
  runWriter (runExceptT mb)

addImport :: ModuleName -> CST.Import -> ModuleBuilder Unit
addImport moduleName import_ =
  modBuilder (_ { imports = [ moduleName /\ import_ ]
                }
             )

exportAll :: ModuleBuilder Unit
exportAll =
  modBuilder (_ { exports = ExportAll
                }
             )

addCSTExport :: CST.Export -> ModuleBuilder Unit
addCSTExport export =
  modBuilder (_ { exports = ExportSelected [export]
                }
             )


addCSTDeclaration :: CST.Declaration -> ModuleBuilder Unit
addCSTDeclaration decl = do
  modBuilder (_ { decls = [decl]
                }
             )

addForeignBinding :: String -> ModuleBuilder Unit
addForeignBinding b =
  modBuilder (_ { foreignBinding = Just b
                }
             )

modBuilder :: (ModuleBuilderState -> ModuleBuilderState) -> ModuleBuilder Unit
modBuilder f = tell (f emptySt)

emptySt :: ModuleBuilderState
emptySt = mempty

-- Names

mkName ::
  forall n.
  ReadName n =>
  AsNameError n =>
  String ->
  ModuleBuilder n
mkName = readName'

mkQualName ::
  forall n.
  AsImport n =>
  UnwrapQualName n =>
  ReadName n =>
  AsNameError n =>
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

-- Utils

buildImportDecls :: Array (ModuleName /\ CST.Import) -> Array CST.ImportDecl
buildImportDecls ps =
  preludeImportDecls <> nonPreludeImportDecls
  where
    { yes: preludeImports
    , no: nonPreludeImports
    } = Array.partition (eq preludeCSTModuleName <<< fst) ps

    preludeImportDecls =
      if Array.null preludeImports
      then []
      else [preludeImportDecl]

    nonPreludeImportDecls = buildNonPreludeImportDecls nonPreludeImports

buildNonPreludeImportDecls :: Array (ModuleName /\ CST.Import) -> Array CST.ImportDecl
buildNonPreludeImportDecls ps =
  uncurry toImportDecl <$> Map.toUnfoldable importMap
  where
    importMap = foldr addPair Map.empty ps

    addPair (m /\ i) = Map.alter (Just <<< addImportE i <<< fold) m

    addImportE i@(CST.ImportType n (Just _)) s =
      Set.insert i <<< Set.delete (CST.ImportType n Nothing) $ s
    addImportE i@(CST.ImportType n Nothing) s =
      if Set.member (CST.ImportType n (Just CST.DataAll)) s
      then s
      else Set.insert i s
    addImportE i s =
      Set.insert i s

    toImportDecl moduleName names =
      CST.ImportDecl { moduleName
                     , names: Set.toUnfoldable names
                     , qualification: Nothing
                     }

preludeImportDecl :: CST.ImportDecl
preludeImportDecl =
  CST.ImportDecl { moduleName: preludeCSTModuleName
                 , names: []
                 , qualification: Nothing
                 }

preludeCSTModuleName :: CST.ModuleName
preludeCSTModuleName =
  CST.ModuleName $ NonEmptyArray.singleton $ CST.ProperName "Prelude"
