{-# LANGUAGE CPP #-}
module SoOSiM.Examples.Loader where

import qualified Data.HashMap.Strict as HashMap
import qualified System.FilePath     as FilePath

import SoOSiM
import SoOSiM.Types

import SoOSiM.Components.ApplicationHandler
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.ProcManager
import SoOSiM.Components.ResourceManager
import SoOSiM.Components.Scheduler

import SoOSiM.Examples.Parser

#ifdef CABAL
import Paths_SoOSiM_components
#else
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . ("../" ++)
#endif

loader :: FilePath -> IO SimState
loader f = do
  exampleDir       <- getDataFileName "examples"
  let example1File = FilePath.combine exampleDir (f ++ ".json")
  (Example app rs) <- readExample example1File

  initSim $ do
    traceMsg "Start the application handler"
    ahId <- applicationHandler
    addPrograms ahId (HashMap.singleton f app)

    traceMsg "Start the resource manager"
    rmId <- resourceManager
    sequence_ $ zipWith
                  (\(Resource _ resT) r -> do
                      rId <- r
                      addResource rmId rId resT
                  ) rs (getNodeId:(repeat createNode))

    traceMsg "Start the process manager"
    pmId <- processManager rmId

    traceMsg "Start the example1 application"
    runProgram pmId f

    return ()

