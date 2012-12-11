{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module SoOSiM.Components.Thread.Behaviour where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Lens
import Control.Monad
import Data.Maybe

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.Scheduler.Interface (threadCompleted)

import SoOSiM.Components.Thread.Interface
import SoOSiM.Components.Thread.Types

data TH_State
  = TH_State
  { _actual_id       :: ThreadId
  , _sched_id        :: ComponentId
  , _thread_state    :: Maybe (TVar Thread)
  }

makeLenses ''TH_State

data TH_Cmd
  = TH_Start
  deriving Typeable

data TH_Msg
  = TH_Void
  deriving Typeable

threadIState :: TH_State
threadIState = TH_State (-1) (-1) Nothing

threadBehaviour ::
  TH_State
  -> Input TH_Cmd
  -> Sim TH_State
threadBehaviour s@(TH_State _ _ Nothing) _ = yield s

threadBehaviour s _ = do
  traceMsg "Starting computation"
  let ts = fromJust $ s ^. thread_state
  t <- runSTM $ readTVar ts
  case (t ^. execution_state) of
    Executing -> do
      -- Read from inputs ports
      runSTM $ mapM_ readTQueue (t ^. in_ports)

      -- Execute computation
      compute (t ^. exec_cycles) ()

      -- Write to output ports
      runSTM $ mapM_ (\(_,q) -> writeTQueue q ()) (t^.out_ports)

      -- Signal scheduler that thread has completed
      runSTM $ modifyTVar' ts (execution_state .~ Waiting)
      threadCompleted (s ^. sched_id) (s ^. actual_id)
      traceMsg "Finished computation"
      stop

    -- Waiting to start
    Waiting -> do
      traceMsg "Waiting"
      return s

    -- Finished one execution cycle
    Blocked -> do
      traceMsg "Stopping"
      stop
