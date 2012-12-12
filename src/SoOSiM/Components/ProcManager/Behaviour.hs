{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module SoOSiM.Components.ProcManager.Behaviour
  (procMgr)
where

import Control.Arrow                 (second)
import Control.Applicative
import Control.Concurrent.STM.TVar   (newTVar)
import Control.Concurrent.STM.TQueue (newTQueue,writeTQueue)
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Foldable       as F
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict     as Map
import Data.Maybe (fromJust)
import qualified Data.Traversable as T

import SoOSiM
import SoOSiM.Components.ApplicationHandler
import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.ResourceManager
import SoOSiM.Components.ResourceManager.Types
import SoOSiM.Components.SoOSApplicationGraph
import SoOSiM.Components.Scheduler
import SoOSiM.Components.Thread

import SoOSiM.Components.ProcManager.Interface
import SoOSiM.Components.ProcManager.Types

procMgr ::
  PM_State
  -> Input PM_Cmd
  -> Sim PM_State
procMgr s i = execStateT (behaviour i) s

type ProcMgrM a = StateT PM_State Sim a

behaviour ::
  Input PM_Cmd
  -> ProcMgrM ()
behaviour (Message (RunProgram fN) retAddr) = do
  -- invokes the Application Handler
  thread_graph <- lift $ applicationHandler >>= flip loadProgram fN

  -- Now we have to contact the Resource Manager

  -- prepares a list of resource requirements,
  -- for each resource description this list contains
  -- the number of desired resources
  let rl = prepareResourceRequestListSimple thread_graph

  -- the resource manager for this Process manager should be
  -- uniquely identified. here I am assuming that we have a
  -- singleton implementation of the resource manager for this file:
  -- function instance() will locate the resource manager for this
  -- instance of the process manager.
  rId  <- use rm
  pmId <- lift $ getComponentId
  res  <- lift $ requestResources rId pmId rl

  -- Now, if necessary I should allocate threads to resources. in
  -- this first sample implementation, I ignore the content of the
  -- resource descriptors, and I assume that all thread and
  -- resources have the correct ISA.

  -- Create all threads
  let threads = HashMap.fromList
              $ map (\v -> (v_id v, newThread (v_id v) (executionTime v)))
              $ vertices thread_graph

  tbqueues <- lift $ runSTM $ replicateM (length $ edges thread_graph) newTQueue

  -- Make connections
  (threads',[]) <- F.foldrM
         (\e (t,(q:qs)) -> do
            lift $ runSTM $ replicateM (n_tokens e) (writeTQueue q ())
                -- Create the in_port of the destination thread, and
                -- initialize it with the number of tokens
            let t'  = if (end e < 0)
                        then t
                        else HashMap.adjust (in_ports %~ (++ [q])) (end e) t

                -- create the out_ports of the source thread, and
                -- initialize it with the pair (thread_id, destination port)
                t'' = if (start e < 0)
                        then t'
                        else HashMap.adjust (out_ports %~ (++ [(end e,q)])) (start e) t'
            return (t'',qs)
         )
         (threads,tbqueues)
       $ edges thread_graph

  -- Now initialize the scheduler, passing the list of
  -- threads, the list of resources, and the mapping

  -- create the list of resources
  rc <- mapM (\x ->
                do d <- use rm >>= (\rId -> lift $ getResourceDescription rId x)
                   return (x,fromJust d)
             ) res

  -- Allocation algorithms. Here I just statically allocate
  -- threads to resources in the simplest possible way
  let th_all = allocate_simple threads' rc
  lift $ traceMsg $ "ThreadAssignment: " ++ show th_all

  -- Another alternative allocation strategy
  -- let th_all = allocate_global threads' res

  -- Now initialize the scheduler, passing the list of
  -- threads, and the list of resources
  threads'' <- T.mapM (lift . runSTM . newTVar) threads'
  pmId <- lift $ getComponentId
  sId  <- lift $ scheduler pmId
  lift $ initScheduler sId threads'' rc th_all

  lift $ respond ProcManager retAddr PM_Void

behaviour (Message TerminateProgram retAddr) = do
  -- The program has completed, free the resources
  pmId <- lift $ getComponentId
  rId  <- use rm
  res  <- lift $ freeResources rId pmId

  lift $ respond ProcManager retAddr PM_Void

behaviour _ = return ()

prepareResourceRequestListSimple ::
  ApplicationGraph
  -> ResourceRequestList
prepareResourceRequestListSimple ag = rl
  where
    -- anyRes is a constant that means "give me any resource that you have"

    -- this will ask for a number of processors equal to the number of threads
    rl = [(anyRes,numberOfVertices ag)]

allocate_simple ::
  HashMap ThreadId Thread
  -> [(ResourceId,ResourceDescriptor)]
  -> (HashMap ThreadId [ResourceId])
allocate_simple threads resMap = thAll
  where
    -- Build the inverse of resMap
    resMapI = Map.toList $ foldl
                (\m (rId,r) ->
                  Map.alter
                    (\case
                      Nothing -> Just [rId]
                      Just rs -> Just (rId:rs)
                    ) r m
                ) Map.empty resMap

    -- Load-balance resource assignment
    thAll   = snd $ T.mapAccumL
                (\m t -> second (:[]) $ assignResource t m)
                resMapI
                threads

    assignResource ::
      Thread
      -> [(ResourceDescriptor,[ResourceId])]
      -> ([(ResourceDescriptor,[ResourceId])],ResourceId)
    assignResource t [] = error "No assignable resource found"
    assignResource t (rm@(r,(rId:rIds)):rms)
      -- If a compatible resource is found, assign the resource
      -- and rotate the resourceId list to balance the assignment of
      -- threads to resources
      | isComplient r (t^.rr) = ((r,rIds ++ [rId]):rms,rId)
      | otherwise             = let (rms',rId) = assignResource t rms
                                in  (rm:rms',rId)
