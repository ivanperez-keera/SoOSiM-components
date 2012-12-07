{-# LANGUAGE TemplateHaskell #-}
module SoOSiM.Components.Thread.Types where

import Control.Lens

import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor

data ThreadState = Blocked | Waiting | Executing

data Thread
  = Thread
  { _threadId        :: ThreadId
  , _n_in            :: Int
  , _n_out           :: Int
  , _in_ports        :: [Int]
  , _out_ports       :: [(ThreadId,Int)]
  , _exec_cycles     :: Int
  , _rr              :: ResourceDescriptor
  , _execution_state :: ThreadState
  , _res_id          :: ResourceId
  , _activation_time :: Int
  }

makeLenses ''Thread

