module CST.Simple.TestUtils
       ( build
       , buildA
       , build'
       , buildModuleErr
       , requireOne
       , requireMatch
       , shouldImport
       , shouldMatchCST
       , shouldBeEquiv
       , fooBarModuleName
       , intCSTType
       , stringCSTType
       , cstTypCons
       , cstUnqualProperName
       , cstUnqualIdent
       , cstUnqualOpName
       , cstUnqualName
       ) where

import Prelude

import CST.Simple.Internal.CodegenError (CodegenError)
import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, runModuleBuilder)
import CST.Simple.Names (ModuleName)
import CST.Simple.Types (ModuleContent)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\))
import Effect.Exception (Error, error)
import Language.PS.CST as CST
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldReturn)

build :: forall m a. MonadThrow Error m => ModuleBuilder a -> m ModuleContent
build = map snd <<< build'

buildA :: forall m a. MonadThrow Error m => ModuleBuilder a -> m a
buildA = map fst <<< build'

build' :: forall m a. MonadThrow Error m => ModuleBuilder a -> m (a /\ ModuleContent)
build' mb = case runModuleBuilder mb of
  Left e ->
    throwError $ error $ "codegen error - " <> show e
  Right r ->
    pure r

buildModuleErr :: forall m a. MonadThrow Error m => ModuleBuilder a -> m CodegenError
buildModuleErr mb = case runModuleBuilder mb of
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
  a1' <- build' (run a1)
  a2' <- build' (run a2)
  a1' `shouldEqual` a2'

fooBarModuleName :: ModuleName
fooBarModuleName =
  CST.ModuleName $ NonEmptyArray.cons' (CST.ProperName "Foo") [ CST.ProperName "Bar" ]

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
