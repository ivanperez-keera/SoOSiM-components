{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.Thread.Interface where

import Control.Concurrent.STM.TVar (TVar)
import Control.Lens ((^.))

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor

import {-# SOURCE #-} SoOSiM.Components.Thread.Behaviour
import SoOSiM.Components.Thread.Types

newtype ThreadIFace = ThreadIFace ThreadId

instance ComponentInterface ThreadIFace where
  type State ThreadIFace          = TH_State
  type Receive ThreadIFace        = TH_Cmd
  type Send ThreadIFace           = TH_Msg
  initState                       = const threadIState
  componentName (ThreadIFace tid) = ("Thread: " ++ show tid)
  componentBehaviour              = const threadBehaviour

-- | Create a new thread
newThread ::
  ThreadId  -- ^ ThreadId
  -> Int    -- ^ Number of cycles needed to execute
  -> Thread
newThread tId exec = Thread tId 0 0 [] [] exec anyRes Blocked (-1) 0

-- | Create a new thread body / instance
threadInstance ::
  ThreadId       -- ^ (copy of) ThreadId
  -> ComponentId -- ^ ComponentID of the scheduler controlling the thread
  -> TVar Thread -- ^ Reference to the thread meta-data
  -> NodeId      -- ^ Node on which to instantiate the thread
  -> Sim ()
threadInstance tid sid th nid = do
  _ <- createComponentNPS (Just nid) Nothing (Just $ TH_State tid sid (Just th)) (ThreadIFace tid)
  return ()
