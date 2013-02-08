module SoOSiM.Components.ApplicationHandler.Behaviour where

import qualified Data.HashMap.Strict as HashMap

import SoOSiM
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.SoOSApplicationGraph

import SoOSiM.Components.ApplicationHandler.Interface
import SoOSiM.Components.ApplicationHandler.Types

appHandler ::
  AH_State
  -> Input AH_Cmd
  -> Sim AH_State
appHandler s@(AH_State appMap) (Message _ (LoadProgram fn) retAddr) = do
  respond ApplicationHandler retAddr (AH_AG $ appMap HashMap.! fn)
  yield s

appHandler (AH_State _) (Message _ (AddApps appMap) retAddr) =
  yield (AH_State appMap)

appHandler s _ = yield s
