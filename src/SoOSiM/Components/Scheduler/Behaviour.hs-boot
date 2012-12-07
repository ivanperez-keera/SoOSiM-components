module SoOSiM.Components.Scheduler.Behaviour
  (sched)
where

import SoOSiM
import SoOSiM.Components.Scheduler.Types

sched ::
  SC_State
  -> Input SC_Cmd
  -> Sim SC_State

