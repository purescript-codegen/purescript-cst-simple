module CST.Simple.Project
       ( writeProject
       ) where

import Prelude

import CST.Simple.Types (ProjectSettings, Project)
import Effect.Aff (Aff)

writeProject :: ProjectSettings -> Project -> Aff Unit
writeProject _ _ = pure unit
