module SoOSiM.Components.ResourceManager.Behaviour
  (resourceManager)
where

import SoOSiM
import SoOSiM.Components.ResourceManager.Types

resourceManager ::
  RM_State
  -> Input RM_Cmd
  -> Sim RM_State
