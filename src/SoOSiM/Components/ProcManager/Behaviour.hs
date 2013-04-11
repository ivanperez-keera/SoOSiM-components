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
import Control.Monad.Writer
import           Data.Char           (toLower)
import qualified Data.Foldable       as F
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as L
import qualified Data.Map            as Map
import Data.Maybe (isJust,fromJust,mapMaybe,fromMaybe)
import           Data.Ord
import qualified Data.Traversable as T

import qualified SoOSiM
import SoOSiM hiding (traceMsg)
import SoOSiM.Components.ApplicationHandler
import SoOSiM.Components.Common
import SoOSiM.Components.MemoryManager
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
  lift $ traceMsgTag ("Begin application " ++ fN) ("AppBegin " ++ fN)
  -- invokes the Application Handler
  thread_graph <- lift $ applicationHandler >>= flip loadProgram fN

  -- Create all threads
  let threads = HashMap.fromList
              $ map (\v -> ( v_id v
                           , newThread (v_id v)
                                       (executionTime v)
                                       (appCommands v)
                                       (memRange v)
                           ))
              $ vertices thread_graph

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
                     return (x,maybe (error "fromJust: resources") id d)
               ) res

    -- Allocation algorithms. Here I just statically allocate
    -- threads to resources in the simplest possible way
    -- let th_allM = allocate threads rc assignResourceSimple ()
    let th_allM = case (fmap (map toLower) $ allocSort thread_graph) of
                    Just "minwcet" -> allocate threads rc assignResourceMinWCET 0
                    _              -> allocate threads rc assignResourceSimple ()

    -- Another alternative allocation strategy
    -- let th_allM = allocate_global threads' res
    return $ fmap (,rc) th_allM

  -- Make connections
  tbqueues <- lift $ runSTM $ replicateM (length $ edges thread_graph) newTQueue
  startTime <- lift $ getTime
  ((threads',[]), (periodicEdges,deadlineEdges)) <- runWriterT $ F.foldrM
         (\e (t,(q:qs)) -> do
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

            -- Instantiate periodic edges
            case (periodic e) of
              Nothing -> lift $ lift $ runSTM $ replicateM_ (n_tokens e) (writeTQueue q startTime)
              Just p  -> case n_tokens e of
                            0 -> return ()
                            n -> do
                              lift $ lift $ runSTM $ writeTQueue q startTime
                              tell ([(q,0,p,n-1)],[])

            -- Instantiate deadline edges
            case (deadline e) of
              Nothing -> return ()
              Just n  -> tell ([],[(q,n,fN,start e)])

            return (t'',qs)
         )
         (threads,tbqueues)
       $ edges thread_graph

  traceMsg $ "ThreadAssignment(" ++ fromMaybe "SIMPLE" (allocSort thread_graph) ++ "): "  ++ show th_all
  periodicEdgesS <- lift $ runSTM $ newTVar periodicEdges

  mmMasterId <- lift $ createMemoryManager Nothing Nothing

  -- Instantiate memory managers on those nodes where threads are assigned
  forM_ (HashMap.toList th_all) $ \(tId,(rId:_)) ->
    do mmId <- lift $ createMemoryManager (Just rId) (Just mmMasterId)
       let (b,s) = (threads HashMap.! tId) ^. localMem
       unless (b == 0 && s == 0) $ lift $ registerMem (Just mmId) b s

  -- Now initialize the scheduler, passing the list of
  -- threads, and the list of resources
  threads'' <- T.mapM (lift . runSTM . newTVar) threads'
  pmId <- lift $ getComponentId
  sId  <- lift $ newScheduler pmId
  traceMsg $ "Starting scheduler"
  lift $ initScheduler sId threads'' rc th_all (schedulerSort thread_graph) fN periodicEdgesS

  -- Initialize Periodic I/O if needed
  unless (null periodicEdges && null deadlineEdges) $ do
    let pIOState = PeriodicIOS (periodicEdgesS,deadlineEdges,sId)
    newId <- lift $ SoOSiM.createComponentNPS Nothing Nothing (Just pIOState) (PeriodicIO fN)
    pIO .= newId


behaviour (Message _ TerminateProgram retAddr) = do
  fN <- fmap appName $ use thread_graph

  -- The program has completed, free the resources
  pmId <- lift $ getComponentId
  rId  <- use rm
  res  <- lift $ freeResources rId pmId

  -- Stop the scheduler
  lift $ stopScheduler (returnAddress retAddr)

  -- Stop the periodic IO
  pIOid <- use pIO
  unless (pIOid < 0) (lift $ stopPIO pIOid fN)

  -- Stop the process manager
  lift stop

behaviour _ = return ()

prepareResourceRequestListSimple ::
  ApplicationGraph
  -> ResourceRequestList
prepareResourceRequestListSimple ag = rl
  where
    -- anyRes is a constant that means "give me any resource that you have"

    -- this will ask for a number of processors equal to the number of threads
    rl = [(ANY_RES,numberOfVertices ag)]

allocate ::
  HashMap ThreadId Thread
  -> [(ResourceId,ResourceDescriptor)]
  -> AssignProc a
  -> a
  -> Maybe (HashMap ThreadId [ResourceId])
allocate threads resMap assignResource initR = thAll
  where
    threads' = reverse
             $ L.sortBy (comparing (\(_,t) -> t^.exec_cycles))
             $ HashMap.toList threads
    -- Build the inverse of resMap
    resMapI = Map.toList $ foldl
                (\m (rId,r) ->
                  Map.alter
                    (\x -> case x of
                      Nothing -> Just [(rId,initR)]
                      Just rs -> Just ((rId,initR):rs)
                    ) r m
                ) Map.empty resMap

    -- Load-balance resource assignment
    thAll   = fmap HashMap.fromList $ T.sequence $ snd $ T.mapAccumL
                (\m (t_id,t) -> assignResource t m)
                resMapI
                threads'

type AssignProc a =
  Thread
  -> [(ResourceDescriptor,[(ResourceId,a)])]
  -> ([(ResourceDescriptor,[(ResourceId,a)])],Maybe (ThreadId,[ResourceId]))

assignResourceSimple :: AssignProc ()
assignResourceSimple t [] = ([], Nothing)
assignResourceSimple t (rm@(r,(rId:rIds)):rms)
  -- If a compatible resource is found, assign the resource
  -- and rotate the resourceId list to balance the assignment of
  -- threads to resources
  | isComplient r (t^.rr) = ((r,rIds ++ [rId]):rms,Just $ (t^.threadId,[fst rId]))
  | otherwise             = let (rms',rId) = assignResourceSimple t rms
                            in  (rm:rms',rId)

assignResourceMinWCET :: AssignProc Int
assignResourceMinWCET t rms = (c_rs' ++ other_rs, rIdM)
  where
    (c_rs,other_rs) = L.partition (\(r,_) -> r `isComplient` (t^.rr)) rms
    (rIdM,c_rs')    = case (map (second (L.sortBy maxUtil')) $ L.sortBy maxUtil c_rs) of
                        []                    -> (Nothing,[])
                        ((r,(rId:rIds)):rms)  -> (Just $ (t^.threadId,[fst rId]), (r,(second (+(t^.exec_cycles)) rId:rIds)):rms)

    maxUtil :: (ResourceDescriptor,[(ResourceId,Int)]) -> (ResourceDescriptor,[(ResourceId,Int)]) -> Ordering
    maxUtil (_,rs1) (_,rs2) = compare (map snd rs1) (map snd rs2)

    maxUtil' :: (ResourceId,Int) -> (ResourceId,Int) -> Ordering
    maxUtil' (_,u1) (_,u2) = compare u1 u2


traceMsg = lift . SoOSiM.traceMsg
