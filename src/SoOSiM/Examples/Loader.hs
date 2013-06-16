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
import SoOSiM.Components.MemoryManager
import SoOSiM.Components.Deployer

import SoOSiM.Examples.Parser

loader :: FilePath -> IO SimState
loader f = do
  (Example apps dist rs) <- readExample f

  initSim (res_id $ head rs) $ do
    traceMsg "Start the application handler"
    dmId <- deployer
    ahId <- applicationHandler
    addPrograms ahId (HashMap.fromList (zip (map appName apps) apps))

    traceMsg "Start the resource manager"
    rmId <- resourceManager dist
    sequence_ $ zipWith
                  (\(Resource nId resT) r -> do
                      rId <- r nId
                      addResource rmId rId resT
                  ) rs ((const getNodeId):(repeat (\r -> createNodeN r >> return r)))

    curNodeId <- getNodeId
    mmMaster <- createMemoryManager (Just curNodeId) Nothing
    mmSlaves <- mapM (\(Resource nId _) -> createMemoryManager (Just nId) (Just mmMaster))
                $ tail rs

    forM_ (map appName apps) $ \a -> do
      traceMsg "Start the process manager"
      pmId <- createProcessManager rmId

      traceMsg "Start the application"
      runProgram pmId a
