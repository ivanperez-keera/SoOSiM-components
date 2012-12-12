{-# LANGUAGE TemplateHaskell #-}
module SoOSiM.Components.Thread.Types where

import Control.Lens
import Control.Concurrent.STM.TQueue

import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor

data ThreadState = Blocked | Waiting | Executing

data Thread
  = Thread
  { -- | The thread unique id
    _threadId        :: ThreadId
    -- | number of incoming \"ports\", each in-port has an id from 0 to (n_in - 1)
  , _n_in            :: Int
    -- | number of outgoing \"ports\", each out-port has an id form 0 to (n_in - 1)
  , _n_out           :: Int
    -- | incoming ports: ntokens per port
  , _in_ports        :: [TQueue ()]
    -- | outgoing links
    --
    -- contains the pair (thread_dest_id, in_port_id) of the destination threads
  , _out_ports       :: [(ThreadId,TQueue ())]
    -- | Number of (simulation) cycles needed to complete one instance of the thread
  , _exec_cycles     :: Int
    -- | resource requirements
  , _rr              :: ResourceDescriptor
    -- | an enumerations: BLOCKED; READY; EXECUTING
  , _execution_state :: ThreadState
    -- | The id of the resource where this thread is executing
  , _res_id          :: ResourceId
    -- | It is the SimTime when the current instance has been activated
    -- this is update by the Scheduler.wake_up_threads when the thres is
    -- moved from blocked to ready.
    -- it can be needed to sort ready thread in FIFO order
  , _activation_time :: Int
  }

makeLenses ''Thread
