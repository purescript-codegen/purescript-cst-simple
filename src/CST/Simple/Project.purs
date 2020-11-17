module CST.Simple.Project
       ( runProject
       , defaultProjectSettings
       , writeProject
       ) where

import Prelude

import CST.Simple.ProjectBuilder (ProjectBuilder, buildProject)
import CST.Simple.Types (Project, ProjectSettings, ModuleEntry)
import Data.Either (Either(..))
import Data.Foldable (intercalate, traverse_)
import Data.Newtype (unwrap)
import Dodo as Dodo
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Language.PS.CST as CST
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, mkdir, readdir, rmdir, stat, unlink, writeTextFile)
import Node.FS.Stats (isDirectory)
import Node.Path (FilePath, dirname)
import Node.Path as FilePath

runProject :: ProjectSettings -> ProjectBuilder Unit -> Aff Unit
runProject ps pb = case buildProject pb of
  Left e ->
    Console.error $ "Project error - " <> show e
  Right p ->
    writeProject ps p

defaultProjectSettings :: ProjectSettings
defaultProjectSettings =
  { outputDirectory: "./generated"
  , printOptions: Dodo.twoSpaces
  , rmDirectoryFilesPreRun: false
  }

writeProject :: ProjectSettings -> Project -> Aff Unit
writeProject ps { modules } = do
  log "creating output dir"
  mkdirP ps.outputDirectory
  log "created ouput dir"
  when ps.rmDirectoryFilesPreRun (rmdirFiles ps.outputDirectory)
  traverse_ (writeModuleEntry ps) modules

writeModuleEntry :: ProjectSettings -> ModuleEntry -> Aff Unit
writeModuleEntry ps { cstModule, foreignBinding } = do
  writeFile psFilePath psFileContent
  traverse_ (writeFile jsFilePath) foreignBinding
  where
    fileBaseName = case cstModule of
      CST.Module { moduleName: CST.ModuleName names } ->
        intercalate "." (unwrap <$> names)

    mkFilePath ext = FilePath.concat [ ps.outputDirectory
                                     , fileBaseName <> "." <> ext
                                     ]

    psFilePath = mkFilePath "purs"
    jsFilePath = mkFilePath "js"

    psFileContent = Dodo.print Dodo.plainText ps.printOptions (CST.printModule cstModule)

writeFile :: FilePath -> String -> Aff Unit
writeFile path content = do
  mkdirP (dirname path)
  log $ "Writing " <> path
  writeTextFile UTF8 path content

rmdirFiles :: FilePath -> Aff Unit
rmdirFiles path =
  traverse_ (rmdirOrFileP <<< prefixPath) =<< readdir path

  where
    prefixPath p = FilePath.concat [ path, p ]

rmdirOrFileP :: FilePath -> Aff Unit
rmdirOrFileP path = do
  st <- stat path
  if (isDirectory st)
    then rmdirP path
    else unlink path

rmdirP :: FilePath -> Aff Unit
rmdirP path = do
  rmdirFiles path
  rmdir path

mkdirP :: FilePath -> Aff Unit
mkdirP path = unlessM (exists path) do
  pure unit -- not sure why this is needed. but getting max stack without this
  let baseDir = dirname path
  unlessM (exists baseDir) (mkdirP baseDir)
  mkdir path
