module SoOSiM.Components.Scheduler.Behaviour
  ( sched
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict (StateT,execStateT)
import Control.Monad.Trans.Class  (lift)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (listToMaybe)
import Data.List (delete,sortBy)

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.ProcManager
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.Thread

import SoOSiM.Components.Scheduler.Interface (Scheduler(..))
import SoOSiM.Components.Scheduler.Types

sched ::
  SC_State
  -> Input SC_Cmd
  -> Sim SC_State
sched s i = execStateT (behaviour i) s

behaviour ::
  Input SC_Cmd
  -> StateT SC_State Sim ()
behaviour (Message (Init tl res) retAddr) = do
  mapM_ (\x -> do res_map.at (fst x)   ?= IDLE_RES
                  res_types.at (fst x) ?= (snd x)
        ) res
  thread_list .= tl
  schedule
  lift $ respond Scheduler retAddr SC_Void

behaviour (Message (ThreadCompleted th) retAddr) = do
  -- Block the thread
  thread_list._at th.execution_state .= Blocked
  -- Get the resource where it was executing
  (Just res) <- use (exec_threads.at th)
  -- Remove the thread from the exec_threads list
  exec_threads.at th .= Nothing

  -- Signal that the resource is free
  res_map._at res .= IDLE_RES
  -- Insert thread 'th' into the blocked vector
  blocked %= (++ [th])

  lift $ respond Scheduler retAddr SC_Void
  schedule

behaviour (Message Schedule retAddr) = do
  lift $ respond Scheduler retAddr SC_Void
  schedule

behaviour _ = return ()

schedule :: StateT SC_State Sim ()
schedule = do
  wake_up_threads

  -- If after wake up all threads are blocked, and nobody is
  -- executing, then there is nothing else to do (ALL TOKENS HAVE BEEN
  -- CONSUMED, NOTHING ELSE TO DO)
  whenM ((&&) <$> (uses ready null) <*> (uses exec_threads HashMap.null)) $
    use pm >>= (lift . terminateProgram)

  -- Otherwise, start executing threads on resources
  -- Visit the read list in order of priority
  tIds <- use ready
  forM_ tIds $ \th -> do
    -- Find a suitable resource for the thread
    resM <- find_free_resource th
    maybe' resM (return ()) $ \res -> do
      -- remove from the ready list
      ready                              %= (delete th)
      res_map._at res                    .= BUSY_RES
      thread_list._at th.execution_state .= Executing
      exec_threads._at th                .= res

-- Look at blocked thread, to see if someone needs to be woken up
wake_up_threads :: StateT SC_State Sim ()
wake_up_threads = do
  -- Check ever blocked thread to see if it has at least 1 token in
  -- each incoming link; if so, move the thread from blocked to ready
  tIds <- use blocked
  forM_ tIds (\tid -> do
                Just t <- use (thread_list.at tid)
                -- All input ports should contain at least one token
                when (all (> 0) $ t^.in_ports) $ do
                  -- remove thread from blocked
                  blocked %= (delete tid)
                  -- insert into the ready queue
                  ready   %= (++ [tid])
                  -- Set the arrival time equal to the current simulation time
                  time <- lift $ getTime
                  thread_list._at tid.activation_time .= time
             )

  -- Sort ready list by FIFO (i.e. arrival time)
  use thread_list >>= \tl -> ready %= (sortBy (\a b -> compare (tl HashMap.! a ^. activation_time)
                                                               (tl HashMap.! b ^. activation_time))
                                      )

find_free_resource :: ThreadId -> StateT SC_State Sim (Maybe ResourceId)
find_free_resource th = do
  x   <- use res_types
  thM <- use (thread_list.at th)
  maybe' thM (return Nothing) $ \th -> do
    let rM = listToMaybe . HashMap.keys
           $ HashMap.filter (\r -> isComplient r (th^.rr))
           x
    return rM
