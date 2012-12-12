{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.Scheduler.Interface where

import Control.Concurrent.STM.TVar (TVar)
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
  -> HashMap ThreadId (TVar Thread)
  -> [(ResourceId,ResourceDescriptor)]
  -> HashMap ThreadId [ResourceId]
  -> Sim ()
initScheduler cId th res th_all =
  invokeAsync Scheduler cId (Init th res th_all) ignore

threadCompleted ::
  ComponentId
  -> ThreadId
  -> Sim ()
threadCompleted cId th =
  invokeAsync Scheduler cId (ThreadCompleted th) ignore
