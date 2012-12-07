module SoOSiM.Components.ApplicationHandler.Behaviour
  (appHandler)
where

import SoOSiM
import SoOSiM.Components.ApplicationHandler.Types

appHandler ::
  AH_State
  -> Input AH_Cmd
  -> Sim AH_State
