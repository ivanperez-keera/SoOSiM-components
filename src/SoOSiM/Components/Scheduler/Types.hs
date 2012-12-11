{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module SoOSiM.Components.Scheduler.Types where

import Control.Concurrent.STM.TVar (TVar)
import Control.Lens                (makeLenses)
import Data.HashMap.Strict         (HashMap,empty)

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.Thread

data ResStatus = IDLE_RES | BUSY_RES
  deriving Eq

data SC_State
  = SC_State
  { _pm           :: ComponentId
  , _thread_list  :: HashMap ThreadId (TVar Thread)
  , _ready        :: [ThreadId]
  , _blocked      :: [ThreadId]
  , _exec_threads :: HashMap ThreadId ResourceId
  , _res_map      :: HashMap ResourceId ResStatus
  , _res_types    :: HashMap ResourceId ResourceDescriptor
  }

schedIState :: SC_State
schedIState = SC_State (-1) empty [] [] empty empty empty

makeLenses ''SC_State

data SC_Cmd
  = Init (HashMap ThreadId (TVar Thread)) [(ResourceId,ResourceDescriptor)]
  | ThreadCompleted ThreadId
  | WakeUpThreads
  | FindFreeResources ThreadId
  | Schedule
  deriving Typeable

data SC_Msg
  = SC_Void
  | SC_Node (Maybe ResourceId)
  deriving Typeable
