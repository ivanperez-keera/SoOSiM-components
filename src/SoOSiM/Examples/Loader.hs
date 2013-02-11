module SoOSiM.Examples.Loader where

import Control.Monad
import qualified Data.HashMap.Strict as HashMap

import SoOSiM
import SoOSiM.Types

import SoOSiM.Components.ApplicationHandler
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.ProcManager
import SoOSiM.Components.ResourceManager
import SoOSiM.Components.Scheduler
import SoOSiM.Components.SoOSApplicationGraph

import SoOSiM.Examples.Parser

loader :: FilePath -> IO SimState
loader f = do
  (Example apps rs) <- readExample f

  initSim $ do
    traceMsg "Start the application handler"
    ahId <- applicationHandler
    addPrograms ahId (HashMap.fromList (zip (map appName apps) apps))

    traceMsg "Start the resource manager"
    rmId <- resourceManager
    sequence_ $ zipWith
                  (\(Resource _ resT) r -> do
                      rId <- r
                      addResource rmId rId resT
                  ) rs (getNodeId:(repeat createNode))

    forM_ (map appName apps) $ \a -> do
      traceMsg "Start the process manager"
      pmId <- createProcessManager rmId

      traceMsg "Start the application"
      runProgram pmId a
