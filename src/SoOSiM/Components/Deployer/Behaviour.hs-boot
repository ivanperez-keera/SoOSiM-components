module SoOSiM.Components.Deployer.Behaviour
  (depl)
where

import SoOSiM
import SoOSiM.Components.Deployer.Types

depl ::
  ()
  -> Input DM_Cmd
  -> Sim ()
