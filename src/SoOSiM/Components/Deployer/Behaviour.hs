module SoOSiM.Components.Deployer.Behaviour where

import           Control.Monad       (unless,forM_)
import qualified Data.HashMap.Strict as HashMap

import SoOSiM
import SoOSiM.Components.Deployer.Interface
import SoOSiM.Components.Deployer.Types
import SoOSiM.Components.MemoryManager
import SoOSiM.Components.Thread

depl ::
  ()
  -> Input DM_Cmd
  -> Sim ()
depl s (Message _ (StartThread sId ths) retAddr) = do
  -- Deploy threads on the assigned nodes
  cIds <- mapM (\(thId,thVar,nId,appName,_) -> threadInstance thId sId thVar nId appName) ths

  -- Instantiate memory managers on those nodes where threads are assigned
  mmMasterId <- createMemoryManager Nothing Nothing
  forM_ ths $ \(tId,_,rId,_,(b,s)) ->
    do mmId <- createMemoryManager (Just rId) (Just mmMasterId)
       unless (b == 0 && s == 0) $ registerMem (Just mmId) b s

  respond Deployer retAddr (DM_TH cIds)

  yield s

depl s _ = yield s
