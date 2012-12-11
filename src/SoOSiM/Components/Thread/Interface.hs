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

newThread ::
  ThreadId
  -> Int
  -> Thread
newThread tId exec = Thread tId 0 0 [] [] exec anyRes Blocked (-1) 0

threadInstance ::
  ThreadId
  -> ComponentId
  -> TVar Thread
  -> NodeId
  -> Sim ()
threadInstance tid sid th nid = do
  _ <- createComponentNPS (Just nid) Nothing (Just $ TH_State tid sid (Just th)) (ThreadIFace tid)
  return ()
