module SoOSiM.Examples.Loader where

import qualified Data.HashMap.Strict as HashMap

import SoOSiM
import SoOSiM.Types

import SoOSiM.Components.ApplicationHandler
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.ProcManager
import SoOSiM.Components.ResourceManager
import SoOSiM.Components.Scheduler

import SoOSiM.Examples.Parser

loader :: FilePath -> IO SimState
loader f = do
  (Example app rs) <- readExample f

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
