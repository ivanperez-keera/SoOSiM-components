module SoOSiM.Components.Deployer.Behaviour where

import qualified Data.HashMap.Strict as HashMap

import SoOSiM
import SoOSiM.Components.Deployer.Interface
import SoOSiM.Components.Deployer.Types
import SoOSiM.Components.Thread

depl ::
  ()
  -> Input DM_Cmd
  -> Sim ()
depl s (Message _ (StartThread sId ths) retAddr) = do
  cIds <- mapM (\(thId,thVar,nId,appName) -> threadInstance thId sId thVar nId appName) ths
  respond Deployer retAddr (DM_TH cIds)
  yield s

depl s _ = yield s
