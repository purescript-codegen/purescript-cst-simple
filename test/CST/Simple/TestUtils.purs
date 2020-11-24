module CST.Simple.TestUtils
       ( ModuleContent
       , build
       , buildA
       , build'
       , buildModuleErr
       , requireOne
       , requireMatch
       , shouldImport
       , shouldMatchCST
       , shouldBeEquiv
       , shouldErrorName
       , fooBarModuleName
       , barModuleName
       , intCSTType
       , stringCSTType
       , cstTypCons
       , cstUnqualProperName
       , cstUnqualIdent
       , cstUnqualOpName
       , cstUnqualName
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError(..))
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, buildModule', exportAll)
import CST.Simple.NameFormat (NameFormat)
import CST.Simple.Names (ModuleName)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception (Error, error)
import Language.PS.CST as CST
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldReturn)

type ModuleContent =
  { moduleName :: ModuleName
  , imports :: Array CST.ImportDecl
  , exports :: Array CST.Export
  , declarations :: Array CST.Declaration
  , foreignBinding :: Maybe String
  }

build :: forall m a. MonadThrow Error m => ModuleBuilder a -> m ModuleContent
build = map snd <<< build' true

buildA :: forall m a. MonadThrow Error m => ModuleBuilder a -> m a
buildA = map fst <<< build' true

build' :: forall m a. MonadThrow Error m => Boolean -> ModuleBuilder a -> m (a /\ ModuleContent)
build' isExportAll mb = case buildModule' "Foo" (when isExportAll exportAll *> mb) of
  Left e ->
    throwError $ error $ "codegen error - " <> show e
  Right (a /\ { foreignBinding, cstModule: CST.Module r }) ->
    pure (a /\ { moduleName: r.moduleName
               , imports: r.imports
               , exports: r.exports
               , declarations: r.declarations
               , foreignBinding
               }
         )

buildModuleErr :: forall m a. MonadThrow Error m => ModuleBuilder a -> m CodegenError
buildModuleErr mb = case buildModule' "Foo" (exportAll *> mb) of
  Left e ->
    pure e
  Right r ->
    throwError $ error $ "failed to get a codegen error when one was expected"

requireOne :: forall m a. MonadThrow Error m => Array a -> m a
requireOne [a] = pure a
requireOne as = throwError $ error $ "expected 1 entry. got " <> show (Array.length as) <> " entries"

requireMatch :: forall m a b. Show a => MonadThrow Error m => a -> (a -> Maybe b) -> m b
requireMatch a f = case f a of
  Just b -> pure b
  Nothing -> throwError $ error $ "failed to match - "  <> show a

shouldImport :: forall m a. MonadThrow Error m => ModuleBuilder a -> CST.ImportDecl -> m Unit
shouldImport mb import_ = do
  mod <- build mb
  mod.imports `shouldContain` import_

shouldMatchCST :: forall a cst m. MonadThrow Error m => Show cst => Eq cst => (a -> ModuleBuilder cst) -> a -> cst -> m Unit
shouldMatchCST f a cst = do
  buildA (f a) `shouldReturn` cst

shouldBeEquiv :: forall a b m. MonadThrow Error m => Show b => Eq b => (a -> ModuleBuilder b) -> a -> a -> m Unit
shouldBeEquiv run a1 a2 = do
  a1' <- build' true (run a1)
  a2' <- build' true (run a2)
  a1' `shouldEqual` a2'

shouldErrorName :: forall a m. MonadThrow Error m => ModuleBuilder a -> NameFormat -> m Unit
shouldErrorName mb nf = do
  e <- buildModuleErr mb
  nf' <- requireMatch e $ case _ of
    InvalidName { nameFormat } ->
      Just nameFormat
    _ ->
      Nothing
  nf' `shouldEqual` nf

fooBarModuleName :: ModuleName
fooBarModuleName =
  CST.ModuleName $ NonEmptyArray.cons' (CST.ProperName "Foo") [ CST.ProperName "Bar" ]

barModuleName :: ModuleName
barModuleName =
  CST.ModuleName $ NonEmptyArray.singleton (CST.ProperName "Bar")

intCSTType :: CST.Type
intCSTType =
  CST.TypeConstructor $ CST.QualifiedName
  { qualModule: Nothing
  , qualName: CST.ProperName "Int"
  }

stringCSTType :: CST.Type
stringCSTType =
  CST.TypeConstructor $ CST.QualifiedName
  { qualModule: Nothing
  , qualName: CST.ProperName "String"
  }

cstTypCons :: String -> CST.Type
cstTypCons = CST.TypeConstructor <<< cstUnqualProperName

cstUnqualProperName :: forall p. String -> CST.QualifiedName (CST.ProperName p)
cstUnqualProperName = cstUnqualName <<< CST.ProperName

cstUnqualIdent :: String -> CST.QualifiedName CST.Ident
cstUnqualIdent = cstUnqualName <<< CST.Ident

cstUnqualOpName :: forall p. String -> CST.QualifiedName (CST.OpName p)
cstUnqualOpName = cstUnqualName <<< CST.OpName

cstUnqualName :: forall n. n -> CST.QualifiedName n
cstUnqualName qualName =
  CST.QualifiedName
  { qualModule: Nothing
  , qualName
  }
