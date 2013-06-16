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
import           Data.Function       (on)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as L
import qualified Data.Map            as Map
import Data.Maybe (catMaybes,isJust,fromJust,mapMaybe,fromMaybe)
import           Data.Ord
import qualified Data.Traversable as T

import qualified SoOSiM
import SoOSiM hiding (traceMsg)
import SoOSiM.Components.ApplicationHandler
import SoOSiM.Components.Common
import SoOSiM.Components.Deployer
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
  let deadlines = map (inferDeadline (edges thread_graph)) (vertices thread_graph)
  let threads = HashMap.fromList
              $ zipWith (\v (dO,dI) -> ( v_id v
                                       , newThread (v_id v)
                                                   (executionTime v)
                                                   (appCommands v)
                                                   (memRange v)
                                                   dO
                                                   dI
                                       ))
                (vertices thread_graph) deadlines

  (th_all,rc) <- untilJust $ do
    -- Now we have to contact the Resource Manager

    -- prepares a list of resource requirements,
    -- for each resource description this list contains
    -- the number of desired resources
    let rl = case (recSpec thread_graph) of
               Just rl' -> rl'
               Nothing  -> prepareResourceRequestListSimple thread_graph

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
                    Just "bestfit" -> allocate threads rc assignResourceBestFit 0.0
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
              Nothing -> lift $ lift $ runSTM $ replicateM_ (n_tokens e) (writeTQueue q (startTime,startTime))
              Just p  -> case n_tokens e of
                            0 -> return ()
                            n -> do
                              lift $ lift $ runSTM $ writeTQueue q (startTime,startTime)
                              tell ([(q,0,p,n-1)],[])

            -- Instantiate deadline edges
            case (deadline e) of
              Nothing -> return ()
              Just n  -> tell ([],[(q,n,fN,start e,(-1))])

            return (t'',qs)
         )
         (threads,tbqueues)
       $ edges thread_graph

  traceMsg $ "ThreadAssignment(" ++ fromMaybe "SIMPLE" (allocSort thread_graph) ++ "): "  ++ show th_all
  periodicEdgesS <- lift $ runSTM $ newTVar periodicEdges

  -- Now initialize the scheduler, passing the list of
  -- threads, and the list of resources
  threadVars <- T.mapM (lift . runSTM . newTVar) threads'
  pmId <- lift $ getComponentId
  sId  <- lift $ newScheduler pmId

  -- Deploy all the threads
  dmId <- lift $ deployer
  let thInfo = map (\tId -> ( tId
                            , threadVars HashMap.! tId
                            , head $ th_all HashMap.! tId
                            , fN
                            , (threads HashMap.! tId) ^. localMem
                            ))
                   (HashMap.keys th_all)
  thCIDs <- lift $ deployThreads dmId sId thInfo
  let cmMap = HashMap.fromList $ zip (HashMap.keys th_all) thCIDs

  traceMsg $ "Starting scheduler"
  lift $ initScheduler sId threadVars rc th_all (schedulerSort thread_graph) fN periodicEdgesS cmMap

  -- Initialize Periodic I/O if needed
  unless (null periodicEdges && null deadlineEdges) $ do
    let pIOState = PeriodicIOS (Just periodicEdgesS,deadlineEdges,sId)
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

assignResourceBestFit :: AssignProc Float
assignResourceBestFit th rms = (rsUpdated ++ rsOther, rIdM)
  where
    -- Partition into complient and non-complient resources
    (rsComplient,rsOther) = L.partition (\(resDec,_) -> resDec `isComplient` (th ^. rr)) rms
    -- Determine the utility of the Thread
    thUtil                = threadUtility th
    -- First sort Resource type by best fit
    rsComplientSorted     = map (second (L.sortBy (maxUtil thUtil))) rsComplient
    -- Order the resources
    rsComplientOrdered    = L.sortBy (compareBy (maxUtil thUtil) `on` snd) rsComplientSorted

    -- Assign the resource and update the bin
    (rIdM,rsUpdated)      = case rsComplientOrdered of
                              [] -> (Nothing,[])
                              ((r,(rId:rIds)):rms') -> ( Just (th^.threadId,[fst rId])
                                                       , (r,((second (+thUtil) rId):rIds)):rms'
                                                       )

    maxUtil :: Float -> (ResourceId,Float) -> (ResourceId,Float) -> Ordering
    maxUtil u (_,u1) (_,u2) = let u1' = 1.0 - u1 - u
                                  u2' = 1.0 - u2 - u
                              in case (u1' < 0, u2' < 0) of
                                  (True,True)   -> compare u1' u2'
                                  (True,False)  -> GT
                                  (False,True)  -> LT
                                  (False,False) -> compare u2' u1'

    compareBy :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
    compareBy f [] [] = EQ
    compareBy f [] _  = LT
    compareBy f _  [] = GT
    compareBy f (x:xs) (y:ys) = case f x y of
                                  EQ -> compareBy f xs ys
                                  c  -> c



threadUtility :: Thread -> Float
threadUtility t = case (t ^. relativeDeadlineOut, t ^. relativeDeadlineIn) of
  (Exact d2, Exact d1) -> let dlDiff = d2 - d1
                          in if (dlDiff < 1)
                              then error $ "Thread with ID: " ++ show (t ^. threadId) ++ " has invalid deadlines (inbound,outbound): " ++ show (d1,d2)
                              else (fromIntegral $ t ^. activation_time) / (fromIntegral dlDiff)
  (d1,d2) -> error $ "Thread with ID: " ++ show (t ^. threadId) ++ " has unspecified deadlines (inbound,outbound): " ++ show (d1,d2)

inferDeadline :: [Edge] -> Vertex -> (Deadline,Deadline)
inferDeadline es v = ( case dlsOut of {[] -> Infinity ; (x:_) -> Exact x}
                     , case dlsIn of {[] -> Infinity ; (x:_) -> Exact x}
                     )
  where
    vId    = v_id v
    dlsOut = L.sort . catMaybes $ map deadline (filter ((== vId) . start) es)
    dlsIn  = reverse  . L.sort . catMaybes $ map deadline (filter ((== vId) . end) es)

traceMsg = lift . SoOSiM.traceMsg
