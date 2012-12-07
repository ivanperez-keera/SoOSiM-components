{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.Thread.Interface where

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor

import {-# SOURCE #-} SoOSiM.Components.Thread.Behaviour
import SoOSiM.Components.Thread.Types

data ThreadIFace = ThreadIFace

instance ComponentInterface ThreadIFace where
  type State ThreadIFace   = TH_State
  type Receive ThreadIFace = TH_Cmd
  type Send ThreadIFace    = TH_Msg
  initState                = const threadIState
  componentName            = const "Thread"
  componentBehaviour       = const threadBehaviour

newThread ::
  ThreadId
  -> Int
  -> Thread
newThread tId exec = Thread tId 0 0 [] [] exec anyRes Blocked (-1) 0
