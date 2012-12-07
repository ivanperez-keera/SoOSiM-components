{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.Scheduler.Interface where

import Data.HashMap.Strict (HashMap)

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.Thread

import {-# SOURCE #-} SoOSiM.Components.Scheduler.Behaviour (sched)
import SoOSiM.Components.Scheduler.Types

data Scheduler = Scheduler

instance ComponentInterface Scheduler where
  type State Scheduler   = SC_State
  type Receive Scheduler = SC_Cmd
  type Send Scheduler    = SC_Msg
  initState              = const schedIState
  componentName          = const "Scheduler"
  componentBehaviour     = const sched

scheduler ::
  ComponentId
  -> Sim ComponentId
scheduler p = componentLookup Scheduler >>= \case
    Nothing  -> createComponentNPS Nothing Nothing (Just iState) Scheduler
    Just cId -> return cId
  where
    iState = schedIState { _pm = p }

initScheduler ::
  ComponentId
  -> HashMap ThreadId Thread
  -> [(ResourceId,ResourceDescriptor)]
  -> Sim ()
initScheduler cId th res =
  invokeAsync Scheduler cId (Init th res) ignore
