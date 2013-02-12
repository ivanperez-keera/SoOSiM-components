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
scheduler p = componentLookup Scheduler >>= \x -> case x of
    Nothing  -> createComponentNPS Nothing Nothing (Just iState) Scheduler
    Just cId -> return cId
  where
    iState = schedIState { _pm = p }

newScheduler ::
  ComponentId
  -> Sim ComponentId
newScheduler p = createComponentNPS Nothing Nothing (Just iState) Scheduler
  where
    iState = schedIState { _pm = p }

initScheduler ::
  ComponentId
  -> HashMap ThreadId (TVar Thread)
  -> [(ResourceId,ResourceDescriptor)]
  -> HashMap ThreadId [ResourceId]
  -> Maybe String
  -> String
  -> Sim ()
initScheduler cId th res th_all smM an =
  notify Scheduler cId (Init th res th_all smM an)

stopScheduler ::
  ComponentId
  -> Sim ()
stopScheduler cId = notify Scheduler cId StopSched
