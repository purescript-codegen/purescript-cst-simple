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
import CST.Simple.Names (class AsNameFormat, class ReadImportName, class ReadName, AliasedQualifiedName(..), ConstructorName, ModuleName, QualifiedName, TypedConstructorName(..), aqualName, readName')
import CST.Simple.Types (ModuleEntry)
import Control.Alt (class Alt, (<|>))
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Except.Trans (class MonadThrow)
import Control.Monad.Writer (class MonadTell, class MonadWriter, Writer, runWriter, tell)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either)
import Data.Foldable (any, fold, foldr, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid.Disj (Disj(..))
import Data.Set as Set
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Language.PS.CST as CST

type ModuleBuilderState =
  { imports :: Array ImportEntry
  , exports :: Array CST.Export
  , exportAll :: Disj Boolean
  , decls :: Array CST.Declaration
  , foreignBinding :: Maybe String
  }

type ImportEntry =
  { moduleName :: ModuleName
  , import_ :: CST.Import
  , alias :: Maybe ModuleName
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
  { imports ::
    Array
    { moduleName :: ModuleName
    , import_ :: CST.Import
    , alias :: Maybe ModuleName
    }
  , exports :: Array CST.Export
  , exportAll :: Disj Boolean
  , decls :: Array CST.Declaration
  , foreignBinding :: Maybe String
  } ModuleBuilder
derive newtype instance moduleBuilderMonadWriter ::
  MonadWriter
  { imports ::
    Array
    { moduleName :: ModuleName
    , import_ :: CST.Import
    , alias :: Maybe ModuleName
    }
  , exports :: Array CST.Export
  , exportAll :: Disj Boolean
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
    "(ModuleBuilder " <> show (runModuleBuilder m) <> ")"

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
  let Disj exportAll = c.exportAll
  exports <- getExports c.exports exportAll
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
    getExports [] false = throwError MissingExports
    getExports es false = pure es
    getExports es true =
      if any isModuleExport es
      then throwError IllegalReExportOnExportAll
      else pure []

    isModuleExport (CST.ExportModule _) = true
    isModuleExport _ = false

    toImportDecl moduleName names' =
      CST.ImportDecl { moduleName
                     , names: Set.toUnfoldable names'
                     , qualification: Nothing
                     }

runModuleBuilder :: forall a. ModuleBuilder a -> Either CodegenError a /\ ModuleBuilderState
runModuleBuilder (ModuleBuilder mb) =
  runWriter (runExceptT mb)

addImport :: ModuleName -> CST.Import -> Maybe ModuleName -> ModuleBuilder Unit
addImport moduleName import_ alias =
  modBuilder (_ { imports = [ { moduleName
                              , import_
                              , alias
                              }
                            ]
                }
             )

exportAll :: ModuleBuilder Unit
exportAll =
  modBuilder (_ { exportAll = Disj true
                }
             )

addCSTExport :: CST.Export -> ModuleBuilder Unit
addCSTExport export =
  modBuilder (_ { exports = [export]
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
  ReadImportName n =>
  AsNameFormat n =>
  String ->
  ModuleBuilder n
mkName = readName'

mkQualName ::
  forall n.
  AsImport n =>
  ReadImportName n =>
  ReadName n =>
  AsNameFormat n =>
  String ->
  ModuleBuilder (QualifiedName n)
mkQualName s = do
  AliasedQualifiedName q <- exceptM $ aqualName s
  for_ q.qualModule \qm ->
    addImport qm.moduleName (asImport q.qualName) qm.alias
  pure $ CST.QualifiedName { qualModule: _.alias =<< q.qualModule
                           , qualName: q.qualName
                           }

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

buildImportDecls :: Array ImportEntry -> Array CST.ImportDecl
buildImportDecls ps =
  preludeImportDecls <> nonPreludeImportDecls
  where
    isPreludeImport i =
      i.moduleName == preludeCSTModuleName &&
      isNothing i.alias

    { yes: preludeImports
    , no: nonPreludeImports
    } = Array.partition isPreludeImport ps

    preludeImportDecls =
      if Array.null preludeImports
      then []
      else [preludeImportDecl]

    nonPreludeImportDecls = buildNonPreludeImportDecls nonPreludeImports

buildNonPreludeImportDecls :: Array ImportEntry -> Array CST.ImportDecl
buildNonPreludeImportDecls ps =
  uncurry toImportDecl <$> Map.toUnfoldable importMap
  where
    importMap = foldr addEntry Map.empty ps

    addEntry p =
      Map.alter
      (Just <<< addImportE p.import_ <<< fold)
      { moduleName: p.moduleName
      , alias: p.alias
      }

    addImportE i@(CST.ImportType n (Just _)) s =
      Set.insert i <<< Set.delete (CST.ImportType n Nothing) $ s
    addImportE i@(CST.ImportType n Nothing) s =
      if Set.member (CST.ImportType n (Just CST.DataAll)) s
      then s
      else Set.insert i s
    addImportE i s =
      Set.insert i s

    toImportDecl { moduleName, alias } names =
      CST.ImportDecl { moduleName
                     , names: Set.toUnfoldable names
                     , qualification: alias
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
