{-# LANGUAGE TemplateHaskell #-}
module SoOSiM.Components.Thread.Types where

import Control.Lens
import Control.Concurrent.STM.TQueue

import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor

data ThreadState = Blocked | Waiting | Executing

data Thread
  = Thread
  { _threadId        :: ThreadId
  , _n_in            :: Int
  , _n_out           :: Int
  , _in_ports        :: [TQueue ()]
  , _out_ports       :: [(ThreadId,TQueue ())]
  , _exec_cycles     :: Int
  , _rr              :: ResourceDescriptor
  , _execution_state :: ThreadState
  , _res_id          :: ResourceId
  , _activation_time :: Int
  }

makeLenses ''Thread
