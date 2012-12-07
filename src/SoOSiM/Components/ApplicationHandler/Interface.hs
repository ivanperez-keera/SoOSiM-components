{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.ApplicationHandler.Interface where

import SoOSiM
import {-# SOURCE #-} SoOSiM.Components.ApplicationHandler.Behaviour (appHandler)
import SoOSiM.Components.ApplicationHandler.Types
import SoOSiM.Components.SoOSApplicationGraph

data ApplicationHandler = ApplicationHandler

instance ComponentInterface ApplicationHandler where
  type State ApplicationHandler   = AH_State
  type Receive ApplicationHandler = AH_Cmd
  type Send ApplicationHandler    = AH_Msg
  initState                = const AH_State
  componentName            = const "Application Handler"
  componentBehaviour       = const appHandler

applicationHandler :: Sim ComponentId
applicationHandler = do
  componentLookup ApplicationHandler >>= \case
    Nothing  -> createComponent ApplicationHandler
    Just cId -> return cId

loadProgram :: ComponentId -> String -> Sim ApplicationGraph
loadProgram cId fN = invoke ApplicationHandler cId (LoadProgram fN) >>= (\(AH_AG ag) -> return ag)