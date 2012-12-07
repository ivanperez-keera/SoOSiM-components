module SoOSiM.Components.ApplicationHandler.Behaviour where

import SoOSiM
import SoOSiM.Components.ApplicationHandler.Interface
import SoOSiM.Components.ApplicationHandler.Types
import SoOSiM.Components.SoOSApplicationGraph

appHandler ::
  AH_State
  -> Input AH_Cmd
  -> Sim AH_State
appHandler s (Message (LoadProgram fN) retAddr) = do
  let ag = ApplicationGraph [] []
  respond ApplicationHandler retAddr (AH_AG ag)
  yield s

appHandler s _ = yield s
