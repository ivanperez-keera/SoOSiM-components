module SoOSiM.Components.ProcManager.Behaviour where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)

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

procManager ::
  PM_State
  -> Input PM_Cmd
  -> Sim PM_State
procManager s i = execStateT (procManager' i) s

procManager' ::
  Input PM_Cmd
  -> StateT PM_State Sim ()
procManager' (Message (RunProgram fN) retAddr) = do
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
  rId <- use rm
  aId <- use appId
  res <- lift $ requestResources rId aId rl

  -- Now, if necessary I shoud allocate threads to resources. in
  -- this first sample implementation, I ignore the content of the
  -- resource descriptors, and I assume that all thread and
  -- resources have the correct ISA.

  -- Create all threads
  let threads = HashMap.fromList
              $ map (\v -> (v_id v, newThread (v_id v) (executionTime v)))
              $ vertices thread_graph

  -- Make connections
  let threads'
       = foldr
         (\e t ->
          let index = length $ (t HashMap.! (end e)) ^. in_ports
              -- Create the in_port of the destination thread, and
              -- initialize it with the number of tokens
              t'    = HashMap.adjust (in_ports %~ (++ [n_tokens e])) (end e) t

              -- create the out_ports of the source thread, and
              -- initialize it with the pair (thread_id, destination port)
          in  HashMap.adjust (out_ports %~ (++ [(end e,index)])) (start e) t'
         )
         threads
       $ edges thread_graph

  -- Now initialize the scheduler, passing the list of
  -- threads, and the list of resources
  rc <- mapM (\x -> do d <- use rm >>= (\rId -> lift $ getResourceDescription rId x)
                       return (x,fromJust d)
             ) res

  -- Now initialize the scheduler, passing the list of
  -- threads, and the list of resources
  sId <- use sched
  lift $ invokeAsync Scheduler sId (Init threads' rc) ignore

  lift $ respond ProcManager retAddr PM_Void

procManager' _ = return ()

prepareResourceRequestList ::
  ApplicationGraph
  -> ResourceRequestList
prepareResourceRequestList ag = rl
  where
    rl = [(anyRes,numberOfVertices ag)]
