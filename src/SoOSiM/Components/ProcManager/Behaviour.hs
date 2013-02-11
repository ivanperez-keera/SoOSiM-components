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
import qualified Data.Map            as Map
import Data.Maybe (fromJust,mapMaybe)
import qualified Data.Traversable as T

import qualified SoOSiM
import SoOSiM hiding (traceMsg)
import SoOSiM.Components.ApplicationHandler
import SoOSiM.Components.Common
import SoOSiM.Components.PeriodicIO
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
procMgr s i = execStateT (behaviour i) s >>= yield

type ProcMgrM a = StateT PM_State Sim a

behaviour ::
  Input PM_Cmd
  -> ProcMgrM ()
behaviour (Message _ (RunProgram fN) retAddr) = do
  -- invokes the Application Handler
  thread_graph <- lift $ applicationHandler >>= flip loadProgram fN

  -- Create all threads
  let threads = HashMap.fromList
              $ map (\v -> (v_id v, newThread (v_id v) (executionTime v)))
              $ vertices thread_graph

  tbqueues <- lift $ runSTM $ replicateM (length $ edges thread_graph) newTQueue

  startTime <- lift $ getTime

  -- Make connections
  (threads',[]) <- F.foldrM
         (\e (t,(q:qs)) -> do
            lift $ runSTM $ replicateM (n_tokens e) (writeTQueue q startTime)
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

  (th_all,rc) <- untilJust $ do
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

    -- create the list of resources
    rc <- mapM (\x ->
                  do d <- use rm >>= (\rId -> lift $ getResourceDescription rId x)
                     return (x,fromJust d)
               ) res

    -- Allocation algorithms. Here I just statically allocate
    -- threads to resources in the simplest possible way
    let th_allM = allocate_simple threads' rc

    -- Another alternative allocation strategy
    -- let th_allM = allocate_global threads' res
    return $ fmap (,rc) th_allM

  traceMsg $ "ThreadAssignment: " ++ show th_all

  -- Instantiate periodic edges
  let periodicEdges = mapMaybe (\(e,q) -> case e of
                                  (Edge _ _ _ Nothing _)      -> Nothing
                                  (Edge _ _ _ (Just (p,n)) _) -> Just (q,0,p,n)
                               )
                    $ zip (edges thread_graph) tbqueues

  unless (null periodicEdges) $ do
    let pState = PeriodicIO (periodicEdges,fN)
    lift $ SoOSiM.createComponentNPS Nothing Nothing (Just pState) pState
    return ()

  -- Now initialize the scheduler, passing the list of
  -- threads, and the list of resources
  threads'' <- T.mapM (lift . runSTM . newTVar) threads'
  pmId <- lift $ getComponentId
  sId  <- lift $ scheduler pmId
  traceMsg $ "Starting scheduler"
  lift $ initScheduler sId threads'' rc th_all (schedulerSort thread_graph) fN

behaviour (Message _ TerminateProgram retAddr) = do
  -- The program has completed, free the resources
  pmId <- lift $ getComponentId
  rId  <- use rm
  res  <- lift $ freeResources rId pmId
  return ()

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
  -> Maybe (HashMap ThreadId [ResourceId])
allocate_simple threads resMap = thAll
  where
    -- Build the inverse of resMap
    resMapI = Map.toList $ foldl
                (\m (rId,r) ->
                  Map.alter
                    (\x -> case x of
                      Nothing -> Just [rId]
                      Just rs -> Just (rId:rs)
                    ) r m
                ) Map.empty resMap

    -- Load-balance resource assignment
    thAll   = T.sequence $ snd $ T.mapAccumL
                (\m t -> second (fmap (:[])) $ assignResource t m)
                resMapI
                threads

    assignResource ::
      Thread
      -> [(ResourceDescriptor,[ResourceId])]
      -> ([(ResourceDescriptor,[ResourceId])],Maybe ResourceId)
    assignResource t [] = ([], Nothing)
    assignResource t (rm@(r,(rId:rIds)):rms)
      -- If a compatible resource is found, assign the resource
      -- and rotate the resourceId list to balance the assignment of
      -- threads to resources
      | isComplient r (t^.rr) = ((r,rIds ++ [rId]):rms,Just rId)
      | otherwise             = let (rms',rId) = assignResource t rms
                                in  (rm:rms',rId)

traceMsg = lift . SoOSiM.traceMsg

untilJust ::
  Monad m
  => (m (Maybe a))
  -> m a
untilJust mf = do
  aM <- mf
  case aM of
    Just a  -> return a
    Nothing -> untilJust mf
