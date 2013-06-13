{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.Deployer.Interface where

import Control.Concurrent.STM.TVar (TVar)

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.Thread

import {-# SOURCE #-} SoOSiM.Components.Deployer.Behaviour (depl)
import SoOSiM.Components.Deployer.Types

data Deployer = Deployer

instance ComponentInterface Deployer where
  type State Deployer    = ()
  type Receive Deployer  = DM_Cmd
  type Send Deployer     = DM_Msg
  initState              = const ()
  componentName          = const "Deployer"
  componentBehaviour     = const depl

deployer :: Sim ComponentId
deployer = do
  componentLookup Deployer >>= \x -> case x of
    Nothing  -> createComponent Deployer
    Just cId -> return cId

-- | Create a new thread body / instance
deployThreads ::
  ComponentId
  -> ComponentId     -- ^ ComponentID of the scheduler controlling the thread
  -> [ ( ThreadId    -- ^ (copy of) ThreadId
       , TVar Thread -- ^ Reference to the thread meta-data
       , NodeId      -- ^ Node on which to instantiate the thread
       , String      -- ^ Name of the application the thread belongs to
       ) ]
  -> Sim [ComponentId]
deployThreads dpId sId ths =
  invoke Deployer dpId (StartThread sId ths) >>= (\(DM_TH cids) -> return cids)
