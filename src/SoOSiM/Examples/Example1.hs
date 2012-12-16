{-# LANGUAGE CPP #-}
module SoOSiM.Examples.Example1 where

import qualified System.FilePath     as FilePath

import SoOSiM.Examples.Loader
import SoOSiM.Types

#ifdef CABAL
import Paths_SoOSiM_components
#else
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . ("../" ++)
#endif

simstate :: IO SimState
simstate = do
  exampleDir       <- getDataFileName "examples"
  let example1File = FilePath.combine exampleDir "example1.json"
  loader example1File
