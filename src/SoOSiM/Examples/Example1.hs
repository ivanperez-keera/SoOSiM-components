module SoOSiM.Examples.Example1 where

import SoOSiM
import SoOSiM.Types

import SoOSiM.Components.ApplicationHandler
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.ProcManager
import SoOSiM.Components.ResourceManager
import SoOSiM.Components.Scheduler

simstate :: IO SimState
simstate = initSim $ do
  n0 <- getNodeId
  n1 <- createNode

  traceMsg "Start the resource manager"
  rmId       <- resourceManager
  addResource rmId n0 anyRes
  addResource rmId n1 anyRes

  traceMsg "Start the process manager"
  pmId <- processManager rmId

  traceMsg "Start the example1 application"
  runProgram pmId "example1"

  return ()
