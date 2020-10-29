module CST.Simple.Internal.RecordLabeled
       ( RecordLabeled
       , runRecordLabeled
       , recField
       , recPun
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, ModuleBuilderT, liftModuleBuilder, mkName)
import Language.PS.CST as CST

newtype RecordLabeled a = RecordLabeled (ModuleBuilder (CST.RecordLabeled a))

runRecordLabeled :: forall m a. Monad m => RecordLabeled a -> ModuleBuilderT m (CST.RecordLabeled a)
runRecordLabeled (RecordLabeled mb) = liftModuleBuilder mb

recField :: forall a. String -> a -> RecordLabeled a
recField l a =
  RecordLabeled $ pure $ CST.RecordField (CST.Label l) a

recPun :: forall a. String -> RecordLabeled a
recPun s =
  RecordLabeled $ CST.RecordPun <$> mkName s
