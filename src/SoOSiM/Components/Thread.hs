module SoOSiM.Components.Thread
  (
  -- * Types
    Thread (..)
  , ThreadState (..)
  -- * Creating threads
  , threadInstance
  , newThread
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
  )
where

import SoOSiM.Components.Thread.Interface
import SoOSiM.Components.Thread.Types
