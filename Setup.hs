#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Version

import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity

import Data.Char (isSpace)
import Data.List (dropWhile,reverse, stripPrefix)
import Data.Maybe (fromJust)

main = putStrLn "Run Setup" >> defaultMainWithHooks simpleUserHooks {
  confHook = \pkg flags -> do
  if lookup (FlagName "use-pkg-config") 
            (configConfigurationsFlags flags) == Just True
  then do
    confHook simpleUserHooks pkg flags
  else do
    lbi <- confHook simpleUserHooks pkg flags
    bi <- geosBuildInfo lbi

    return lbi {
      localPkgDescr = updatePackageDescription
                        (Just bi, [("runtests", bi)]) (localPkgDescr lbi)
    }
}

geosBuildInfo :: LocalBuildInfo -> IO BuildInfo
geosBuildInfo lbi = do
  (pgconfigProg, _) <- requireProgram verbosity
                           (simpleProgram "geos-config") (withPrograms lbi)
  let geosconfig = rawSystemProgramStdout verbosity pgconfigProg
  incDir <- geosconfig ["--includes"]
  libDir <- geosconfig ["--ldflags"]
  return emptyBuildInfo {
    extraLibDirs = [strip . fromJust $ stripPrefix "-L" libDir],
    includeDirs  = [strip incDir]
  }
  where
    verbosity = normal -- honestly, this is a hack
    strip x = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse x
