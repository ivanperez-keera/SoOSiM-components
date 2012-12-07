{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module SoOSiM.Components.Thread.Behaviour where

import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Control.Lens
import Control.Monad

import SoOSiM
import SoOSiM.Components.Thread.Interface
import SoOSiM.Components.Thread.Types

data TH_State
  = TH_State
  { _in_port_queues  :: [TBQueue ()]
  , _out_port_queues :: [TBQueue ()]
  , _thread_state    :: Maybe (TVar Thread)
  }

makeLenses ''TH_State

data TH_Cmd
  = TH_Start
  deriving Typeable

data TH_Msg
  = TH_Void
  deriving Typeable

threadIState :: TH_State
threadIState = TH_State [] [] Nothing

threadBehaviour ::
  TH_State
  -> Input TH_Cmd
  -> Sim TH_State
threadBehaviour s@(TH_State _ _ (Just ts)) (Message TH_Start retAddr) = do
  runSTM $ do
    t <- readTVar ts
    case (t ^. execution_state) of
      Blocked   -> return ()
      Waiting   -> return ()
      Executing -> do
        tokenss <- zipWithM (\q i -> replicateM i (readTBQueue q))
                            (s ^. in_port_queues)
                            (t ^. in_ports)

        zipWithM_ (\q i -> replicateM i (writeTBQueue q ()))
                  (s ^. out_port_queues)
                  (t ^. out_ports & fmap snd)

        modifyTVar' ts (execution_state .~ Blocked)

  yield s

threadBehaviour s@(TH_State _ _ Nothing) _ = yield s
