{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.Thread.Interface where

import Control.Concurrent.STM.TVar (TVar)
import Control.Lens ((^.))

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor

import {-# SOURCE #-} SoOSiM.Components.Thread.Behaviour
import SoOSiM.Components.Thread.Types

newtype ThreadIFace = ThreadIFace (ThreadId,String)

instance ComponentInterface ThreadIFace where
  type State ThreadIFace               = TH_State
  type Receive ThreadIFace             = TH_Cmd
  type Send ThreadIFace                = TH_Msg
  initState                            = const threadIState
  componentName (ThreadIFace (tid,an)) = ("<<" ++ an ++ ">>Thread " ++ show tid)
  componentBehaviour                   = const threadBehaviour

-- | Create a new thread
newThread ::
  ThreadId  -- ^ ThreadId
  -> Int    -- ^ Number of cycles needed to execute
  -> Thread
newThread tId exec = Thread tId 0 0 [] [] exec anyRes Killed (-1) 0

-- | Create a new thread body / instance
threadInstance ::
  ThreadId       -- ^ (copy of) ThreadId
  -> ComponentId -- ^ ComponentID of the scheduler controlling the thread
  -> TVar Thread -- ^ Reference to the thread meta-data
  -> NodeId      -- ^ Node on which to instantiate the thread
  -> String      -- ^ Name of the application the thread belongs to
  -> Sim ComponentId
threadInstance tid sid th nid an =
  createComponentNPS (Just nid) Nothing (Just $ TH_State tid sid (Just th) an) (ThreadIFace (tid,an))

startThread ::
  ComponentId
  -> String
  -> ThreadId
  -> Sim ()
startThread cId an tId = notify (ThreadIFace (tId,an)) cId TH_Start

killThread ::
  ComponentId
  -> String
  -> ThreadId
  -> Sim ()
killThread cId an tId = notify (ThreadIFace (tId,an)) cId TH_Stop
