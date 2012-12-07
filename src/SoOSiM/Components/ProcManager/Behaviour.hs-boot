module SoOSiM.Components.ProcManager.Behaviour
  (procMgr)
where

import SoOSiM
import SoOSiM.Components.ProcManager.Types

procMgr ::
  PM_State
  -> Input PM_Cmd
  -> Sim PM_State
