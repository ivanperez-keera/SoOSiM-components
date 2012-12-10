{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.ResourceManager.Interface where

import Data.HashMap.Strict (empty)

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor

import {-# SOURCE #-} SoOSiM.Components.ResourceManager.Behaviour (resourceManager)
import SoOSiM.Components.ResourceManager.Types

data ResourceManager = ResourceManager

instance ComponentInterface ResourceManager where
  type State ResourceManager   = RM_State
  type Receive ResourceManager = RM_Cmd
  type Send ResourceManager    = RM_Msg
  initState                    = const (RM_State empty [] [])
  componentName                = const "Resource Manager"
  componentBehaviour           = const resourceManager

addResource :: ComponentId -> ResourceId -> ResourceDescriptor -> Sim ()
addResource cId rId rd = invoke ResourceManager cId (AddResource rId rd) >>= (\RM_Void -> return ())

requestResources :: ComponentId -> AppId -> ResourceRequestList -> Sim [ResourceId]
requestResources cId appId rl = invoke ResourceManager cId (RequestResources appId rl) >>= (\(RM_Resources r) -> return r)

freeResources :: ComponentId -> AppId -> Sim ()
freeResources cId appId = invoke ResourceManager cId (FreeResources appId) >>= (\RM_Void -> return ())

getResourceDescription :: ComponentId -> ResourceId -> Sim (Maybe ResourceDescriptor)
getResourceDescription cId rId = invoke ResourceManager cId (GetResourceDescription rId) >>= (\(RM_Descriptor r) -> return r)
