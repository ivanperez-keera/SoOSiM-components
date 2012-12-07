module SoOSiM.Components.ProcManager.Behaviour
  (procManager)
where

import SoOSiM
import SoOSiM.Components.ProcManager.Types

procManager ::
  PM_State
  -> Input PM_Cmd
  -> Sim PM_State
