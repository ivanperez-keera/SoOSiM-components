{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SoOSiM.Components.Scheduler.Behaviour
  ( sched
  )
where

import Control.Applicative
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TQueue (isEmptyTQueue)
import Control.Concurrent.STM.TVar (modifyTVar',readTVar)
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict (MonadState,StateT,execStateT)
import Control.Monad.Trans.Class  (lift)
import qualified Data.HashMap.Strict as HashMap
import Data.List (delete,sortBy)
import Data.Maybe (listToMaybe)
import qualified Data.Traversable as T

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.ProcManager
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.Thread

import SoOSiM.Components.Scheduler.Interface (Scheduler(..))
import SoOSiM.Components.Scheduler.Types

newtype Sched a = Sched { runSched :: StateT SC_State Sim a }
  deriving (Applicative, Functor, Monad, MonadState SC_State)

sched ::
  SC_State
  -> Input SC_Cmd
  -> Sim SC_State
sched s i = execStateT (runSched $ behaviour i) s

liftS = Sched . lift

behaviour ::
  Input SC_Cmd
  -> Sched ()
behaviour (Message (Init tl res) retAddr) = do
  mapM_ (\x -> do res_map.at (fst x)   ?= IDLE_RES
                  res_types.at (fst x) ?= (snd x)
        ) res
  thread_list .= tl
  schedule
  liftS $ respond Scheduler retAddr SC_Void

behaviour (Message (ThreadCompleted th) retAddr) = do
  -- Block the thread
  modifyThread (thread_list.at th) (\t -> modifyTVar' t (execution_state .~ Blocked))
  -- Get the resource where it was executing
  (Just res) <- use (exec_threads.at th)
  -- Remove the thread from the exec_threads list
  exec_threads.at th .= Nothing

  -- Signal that the resource is free
  res_map._at res .= IDLE_RES
  -- Insert thread 'th' into the blocked vector
  blocked %= (++ [th])

  liftS $ respond Scheduler retAddr SC_Void
  schedule

behaviour (Message Schedule retAddr) = do
  liftS $ respond Scheduler retAddr SC_Void
  schedule

behaviour _ = return ()

schedule :: Sched ()
schedule = do
  wake_up_threads

  -- If after wake up all threads are blocked, and nobody is
  -- executing, then there is nothing else to do (ALL TOKENS HAVE BEEN
  -- CONSUMED, NOTHING ELSE TO DO)
  finished <- (&&) <$> (uses ready null) <*> (uses exec_threads HashMap.null)
  if finished
    then use pm >>= (liftS . terminateProgram)
    else do
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
        modifyThread (thread_list.at th) (\t -> modifyTVar' t (execution_state .~ Executing))
        exec_threads._at th                .= res
        -- Start the tread on the node
        Just t <- use $ thread_list.at th
        sId    <- liftS $ getComponentId
        liftS $ threadInstance th sId t res

-- Look at blocked thread, to see if someone needs to be woken up
wake_up_threads :: Sched ()
wake_up_threads = do
  -- Check ever blocked thread to see if it has at least 1 token in
  -- each incoming link; if so, move the thread from blocked to ready
  tIds <- use blocked
  forM_ tIds (\tid -> do
                Just t <- readThread (thread_list.at tid)
                -- All input ports should contain at least one token
                inpQueuesEmpty <- liftS $ runSTM $ mapM isEmptyTQueue $ t^.in_ports
                when (all not inpQueuesEmpty) $ do
                  -- remove thread from blocked
                  blocked %= (delete tid)
                  -- insert into the ready queue
                  ready   %= (++ [tid])
                  -- Set the arrival time equal to the current simulation time
                  time <- liftS $ getTime
                  modifyThread (thread_list.at tid) (\t -> modifyTVar' t (activation_time .~ time))
             )

  -- Sort ready list by FIFO (i.e. arrival time)
  readyList <- use thread_list >>= T.mapM (fmap (activation_time ^$) . liftS . runSTM . readTVar)
  ready %= sortBy (\a b -> compare (readyList HashMap.! a)
                                   (readyList HashMap.! b)
                  )
  return ()

find_free_resource :: ThreadId -> Sched (Maybe ResourceId)
find_free_resource th = do
  x   <- use res_types
  thM <- readThread (thread_list.at th)
  maybe' thM (return Nothing) $ \th -> do
    let rM = listToMaybe . HashMap.keys
           $ HashMap.filter (\r -> isComplient r (th^.rr))
           x
    return rM

modifyThread l f = use l >>= (maybe (return ()) (liftS . runSTM . f))
readThread l     = use l >>= (liftS . runSTM . maybe (return Nothing) (fmap Just . readTVar))