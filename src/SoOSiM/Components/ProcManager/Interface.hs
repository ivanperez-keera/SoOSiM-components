{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.ProcManager.Interface where

import SoOSiM
import {-# SOURCE #-} SoOSiM.Components.ProcManager.Behaviour (procMgr)
import SoOSiM.Components.ProcManager.Types

data ProcManager = ProcManager

instance ComponentInterface ProcManager where
  type State ProcManager   = PM_State
  type Receive ProcManager = PM_Cmd
  type Send ProcManager    = PM_Msg
  initState                = const undefined
  componentName            = const "Process Manager"
  componentBehaviour       = const procMgr

processManager :: ComponentId -> Sim ComponentId
processManager r = componentLookup ProcManager >>= \case
    Nothing  -> createComponentNPS Nothing Nothing (Just iState) ProcManager
    Just cId -> return cId
  where
    iState = procMgrIState { _rm = r }

terminateProgram :: ComponentId -> Sim ()
terminateProgram cId = invoke ProcManager cId TerminateProgram >>= (\PM_Void -> return ())

runProgram :: ComponentId -> String -> Sim ()
runProgram cId fN = invoke ProcManager cId (RunProgram fN) >>= (\PM_Void -> return ())
