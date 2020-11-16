module CST.Simple.Internal.RecordLabeled
       ( RecordLabeled
       , runRecordLabeled
       , recField
       , recPun
       ) where

import Prelude

import CST.Simple.Internal.ModuleBuilder (ModuleBuilder, mkName)
import Language.PS.CST as CST

newtype RecordLabeled a = RecordLabeled (ModuleBuilder (CST.RecordLabeled a))

runRecordLabeled :: forall a. RecordLabeled a -> ModuleBuilder (CST.RecordLabeled a)
runRecordLabeled (RecordLabeled mb) = mb

recField :: forall a. String -> a -> RecordLabeled a
recField label a =
  RecordLabeled $ pure $ CST.RecordField (CST.Label label) a

recPun :: forall a. String -> RecordLabeled a
recPun ident =
  RecordLabeled $ CST.RecordPun <$> mkName ident
