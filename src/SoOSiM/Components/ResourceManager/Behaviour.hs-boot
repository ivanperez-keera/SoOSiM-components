module SoOSiM.Components.ResourceManager.Behaviour
  (behaviour)
where

import SoOSiM
import SoOSiM.Components.ResourceManager.Types

behaviour ::
  RM_State
  -> Input RM_Cmd
  -> Sim RM_State
