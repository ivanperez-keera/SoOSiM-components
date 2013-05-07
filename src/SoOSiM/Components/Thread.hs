module SoOSiM.Components.Thread
  (
  -- * Types
    Thread (..)
  , ThreadState (..)
  , Deadline (..)
  -- * Creating threads
  , threadInstance
  , newThread
  -- * Starting and stopping
  , startThread
  , killThread
  -- * Lenses
  , threadId
  , n_in
  , n_out
  , in_ports
  , out_ports
  , exec_cycles
  , rr
  , execution_state
  , res_id
  , activation_time
  , localMem
  )
where

import SoOSiM.Components.Thread.Interface
import SoOSiM.Components.Thread.Types
