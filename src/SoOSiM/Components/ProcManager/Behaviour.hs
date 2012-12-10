module SoOSiM.Components.ProcManager.Behaviour
  (procMgr)
where

import Control.Applicative
import Control.Concurrent.STM.TVar   (newTVar)
import Control.Concurrent.STM.TQueue (newTQueue,writeTQueue)
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
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

behaviour ::
  Input PM_Cmd
  -> StateT PM_State Sim ()
behaviour (Message (RunProgram fN) retAddr) = do
  -- invokes the Application Handler
  thread_graph <- lift $ applicationHandler >>= flip loadProgram fN

  -- Now we have to contact the Resource Manager

  -- prepares a list of resource requirements,
  -- for each resource description this list contains
  -- the number of desired resources
  let rl = prepareResourceRequestList thread_graph

  -- the resource manager for this Process manager shoud be
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
  (threads',[]) <- foldrM
         (\e (t,(q:qs)) -> do
            let -- Create the in_port of the destination thread, and
                -- initialize it with the number of tokens
                t'    = HashMap.adjust (in_ports %~ (++ [q])) (end e) t
            lift $ runSTM $ replicateM (n_tokens e) (writeTQueue q ())

                -- create the out_ports of the source thread, and
                -- initialize it with the pair (thread_id, destination port)
            let t''   =  HashMap.adjust (out_ports %~ (++ [(end e,q)])) (start e) t'
            return (t'',qs)
         )
         (threads,tbqueues)
       $ edges thread_graph

  -- Now initialize the scheduler, passing the list of
  -- threads, and the list of resources
  rc <- mapM (\x -> do d <- use rm >>= (\rId -> lift $ getResourceDescription rId x)
                       return (x,fromJust d)
             ) res

  -- Now initialize the scheduler, passing the list of
  -- threads, and the list of resources
  threads'' <- T.mapM (lift . runSTM . newTVar) threads'
  pmId <- lift $ getComponentId
  sId  <- lift $ scheduler pmId
  lift $ initScheduler sId threads'' rc

  lift $ respond ProcManager retAddr PM_Void

behaviour _ = return ()

prepareResourceRequestList ::
  ApplicationGraph
  -> ResourceRequestList
prepareResourceRequestList ag = rl
  where
    rl = [(anyRes,numberOfVertices ag)]
