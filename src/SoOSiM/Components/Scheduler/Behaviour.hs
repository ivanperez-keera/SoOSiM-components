{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SoOSiM.Components.Scheduler.Behaviour
  ( sched
  )
where

import Control.Applicative
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TQueue (isEmptyTQueue,tryReadTQueue,writeTQueue)
import Control.Concurrent.STM.TVar (modifyTVar',readTVar)
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict (MonadState,StateT(..))
import Control.Monad.Trans.Class  (lift)
import Data.Char
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (delete,sortBy)
import Data.Maybe (catMaybes,listToMaybe,isNothing)
import qualified Data.Traversable as T

import SoOSiM hiding (traceMsg,traceMsgTag)
import qualified SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.ProcManager
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.Thread as Thread

import SoOSiM.Components.Scheduler.Interface (Scheduler(..))
import SoOSiM.Components.Scheduler.Types

newtype Sched a = Sched { runSched :: StateT SC_State Sim a }
  deriving (Applicative, Functor, Monad, MonadState SC_State)

sched ::
  SC_State
  -> Input SC_Cmd
  -> Sim SC_State
sched s i = do
  (c,s') <- runStateT (runSched $ behaviour i) s
  currentTime <- getTime
  let s'' = set last_run currentTime s'
  if c then return s''
       else yield  s''

liftS = Sched . lift

behaviour ::
  Input SC_Cmd
  -> Sched Bool
behaviour (Message _ (Init tl res th_all smM an pE cmMap) retAddr) = do
  thread_list .= tl
  periodic_edges .= (Just pE)
  -- Initializes all resources
  mapM_ (\x -> do res_map.at (fst x)   ?= IDLE_RES
                  res_types.at (fst x) ?= (snd x)
        ) res

  appName .= an
  components .= cmMap

  -- all threads are initially blocked
  blocked               .= (HashMap.keys tl)
  thread_res_allocation .= th_all

  sortingMethod         .= (maybe' smM byArrivalTime $
                              (\sm -> case map toLower sm of
                                  "fifo" -> byArrivalTime
                                  "edf"  -> byDeadline
                                  _      -> error $ "Non-existent scheduling method: " ++ sm
                              ))

  -- Ready! Now we have to call schedule for the first time
  schedule

behaviour (Message _ StopSched _) = liftS stop

behaviour _ = do
  -- Check which threads completed
  tids <- uses thread_list HashMap.keys
  mapM_ thread_completed tids
  -- Schedule threads
  schedule

thread_completed :: ThreadId -> Sched ()
thread_completed th = do
  Just t <- readThread (thread_list.at th)
  when (t ^. execution_state == Waiting) $ do
    -- Block the thread
    modifyThread (thread_list.at th) (\t -> modifyTVar' t (execution_state .~ Blocked))
    -- Get the resource where it was executing
    (Just res) <- use (exec_threads.at th)
    -- Remove the thread from the exec_threads list
    exec_threads.at th .= Nothing

    -- Signal that the resource is free
    res_map.ix res .= IDLE_RES
    -- Insert thread 'th' into the blocked vector
    blocked %= (++ [th])


-- Look at blocked thread, to see if someone needs to be woken up
wake_up_threads :: Sched ()
wake_up_threads = do
  aN  <- use appName
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
                  threadEncMsg tid aN
             )

  threads <- use thread_list >>= T.mapM (liftS . runSTM . readTVar)
  sm      <- use sortingMethod
  ready %= sortBy (\idA idB -> sm (threads HashMap.! idA) (threads HashMap.! idB))

find_free_resource :: ThreadId -> Sched (Maybe ResourceId)
find_free_resource thId = do
  rt_map    <- use res_types
  rm_map    <- use res_map
  resIds    <- use (thread_res_allocation.ix thId)
  (Just th) <- readThread (thread_list.at thId)
  -- The resource is choosen only among the
  -- ones on which the task has been allocated
  let rIdM = listToMaybe
           $ filter
             -- the isCompliant() check is superfluous, since the  process manager has
             -- hopefully already allocated the threads on the compliant
             -- resources
             (\x -> isComplient (th^.rr) (rt_map HashMap.! x) &&
                    (rm_map HashMap.! x) == IDLE_RES
             )
             resIds
  return rIdM

schedule :: Sched Bool
schedule = do
  nId <- liftS $ getNodeId
  aN <- use appName
  runningScheduler aN nId

  -- Sort ready list according to given method
  wake_up_threads

  -- If after wake up all threads are blocked, and nobody is
  -- executing, then there is nothing else to do (ALL TOKENS HAVE BEEN
  -- CONSUMED, NOTHING ELSE TO DO)
  finished <- and <$> T.sequenceA [ uses ready null
                                  , uses exec_threads HashMap.null
                                  , use periodic_edges >>= (maybe (return False) (fmap null . liftS . runSTM . readTVar))
                                  ]
  if finished
    then do
      traceMsgTag ("Application " ++ aN ++ " finished") ("AppFinish " ++ aN)
      killThreads
      use pm >>= (liftS . terminateProgram)
      return False
    else do
      -- Otherwise, start executing threads on resources
      -- Visit the read list in order of priority
      tIds <- use ready
      rm_map <- use res_map

      forM_ tIds $ \th -> do
        -- Find a suitable resource for the thread
        resM <- find_free_resource th
        maybe' resM (waitThreadMsg th aN) $ \res -> do
          startThreadMsg th res aN
          -- remove from the ready list
          ready %= (delete th)
          -- Execute th on res
          res_map.ix res .= BUSY_RES
          exec_threads.at th ?= res

          -- create thread instance on the node if it was killed
          Just t <- use (thread_list.at th)
          t' <- liftS . runSTM $ readTVar t
          when (t'^.execution_state == Killed) $ do
            error $ "Threads shouldn't be killed"
            sId <- liftS $ getComponentId
            cId <- liftS $ threadInstance th sId t res aN
            components.at th ?= cId

          -- Start the tread
          modifyThread (thread_list.at th)
            (\t -> modifyTVar' t ( (execution_state .~ Executing)
                                 . (Thread.res_id .~ res)
                                 )
            )
          use (components.at th) >>=
            maybe (return ()) (\cId -> liftS $ startThread cId aN th)
      return False

killThreads :: Sched ()
killThreads = do
  aN  <- use appName
  tcs <- fmap HashMap.toList $ use components
  forM_ tcs $ \(th,cId) -> liftS $ killThread cId aN th

modifyThread l f = use l >>= (maybe (return ()) (liftS . runSTM . f))
readThread l     = use l >>= (liftS . runSTM . maybe (return Nothing) (fmap Just . readTVar))
traceMsg         = liftS . SoOSiM.traceMsg
traceMsgTag      = (liftS .) . SoOSiM.traceMsgTag

onlyOnce m = do
  currentTime <- liftS $ getTime
  lr <- use last_run
  when (lr < currentTime) m

runningScheduler aN res  = onlyOnce $ traceMsgTag ("Running " ++ aN ++ " scheduler on Node " ++ show res) ("SchedulerExec " ++ aN ++ " Proc" ++ show res)
waitThreadMsg th aN      = onlyOnce $ traceMsgTag ("Thread " ++ show th ++ " is waiting") ("ThreadWait " ++ aN ++ ".T" ++ show th)
startThreadMsg th res aN = traceMsgTag ("Starting thread " ++ show th ++ " on Node " ++ show res) ("ThreadStart " ++ aN ++ ".T" ++ show th ++ " Proc" ++ show res)
threadEncMsg th aN       = traceMsgTag ("Thread " ++ show th ++ " ready to start") ("ThreadEnqueued " ++ aN ++ ".T" ++ show th)
