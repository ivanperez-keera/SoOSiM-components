{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module SoOSiM.Components.Scheduler where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.State.Strict
import Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (sortBy,delete)
import Data.Maybe

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.ProcManager
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.Thread

data ResStatus = IDLE_RES | BUSY_RES

data Scheduler = Scheduler

data SC_State
  = SC_State
  { _pm           :: ComponentId
  , _aapThread    :: Thread
  , _thread_list  :: HashMap ThreadId (Thread)
  , _ready        :: [ThreadId]
  , _blocked      :: [ThreadId]
  , _exec_threads :: HashMap ThreadId ResourceId
  , _res_map      :: HashMap ResourceId ResStatus
  , _res_types    :: HashMap ResourceId ResourceDescriptor
  }

makeLenses ''SC_State

data SC_Cmd
  = Init (HashMap ThreadId (Thread)) [(ResourceId,ResourceDescriptor)]
  | ThreadCompleted ThreadId
  | WakeUpThreads
  | FindFreeResources ThreadId
  | Schedule
  deriving Typeable

data SC_Msg
  = SC_Void
  | SC_Node (Maybe ResourceId)
  deriving Typeable

instance ComponentInterface Scheduler where
  type State Scheduler   = SC_State
  type Receive Scheduler = SC_Cmd
  type Send Scheduler    = SC_Msg
  initState              = const undefined
  componentName          = const "Scheduler"
  componentBehaviour     = const scheduler

scheduler ::
  SC_State
  -> Input SC_Cmd
  -> Sim SC_State
scheduler s i = execStateT (scheduler' i) s

scheduler' ::
  Input SC_Cmd
  -> StateT SC_State Sim ()
scheduler' (Message (Init tl res) retAddr) = do
  mapM_ (\x -> do res_map   %= (at (fst x) ?~ IDLE_RES)
                  res_types %= (at (fst x) ?~ (snd x))
        ) res
  thread_list .= tl
  lift $ respond Scheduler retAddr SC_Void
  -- schedule

scheduler' (Message (ThreadCompleted th) retAddr) = do
  -- Block the thread
  thread_list.at th %= fmap (execution_state .~ Blocked)
  -- Get the resource where it was executing
  (Just res) <- use (exec_threads.at th)
  -- Remove the thread from the exec_threads list
  exec_threads.at th .= Nothing

  -- Signal that the resource is free
  res_map.at res ?= IDLE_RES
  -- Insert thread 'th' into the blocked vector
  blocked        %= (++ [th])

  lift $ respond Scheduler retAddr SC_Void
  -- schedule

scheduler' (Message Schedule retAddr) = do
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
      ready              %= (delete th)
      res_map.at res     ?= BUSY_RES
      thread_list.at th  %= fmap (execution_state .~ Executing)
      exec_threads.at th ?= res


scheduler' _ = return ()

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
                  thread_list.at tid %= fmap (activation_time .~ time)
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

