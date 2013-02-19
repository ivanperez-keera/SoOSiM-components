{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.PeriodicIO where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad
import Data.Maybe

import SoOSiM
import SoOSiM.Components.Common

newtype PeriodicIO = PeriodicIO (TVar [(TQueue Int,Int,Int,Int)],[(TQueue Int,Int)],String)

data PIO_Cmd = PIO_Stop
  deriving Typeable

instance ComponentInterface PeriodicIO where
  type State PeriodicIO               = PeriodicIO
  type Receive PeriodicIO             = PIO_Cmd
  type Send PeriodicIO                = ()
  initState                           = const (PeriodicIO (undefined,[],""))
  componentName (PeriodicIO (_,_,an)) = ("<<" ++ an ++ ">>Periodic IO")
  componentBehaviour                  = const periodicIO

periodicIO ::
  PeriodicIO
  -> Input PIO_Cmd
  -> Sim PeriodicIO
periodicIO (PeriodicIO (_,_,_)) (Message _ PIO_Stop _) = stop

periodicIO s@(PeriodicIO (qsS,ds,n)) _ = do
  currentTime <- getTime
  qs  <- runSTM $ readTVar qsS
  qs' <- fmap catMaybes $ forM qs $ \(q,c,p,n) -> do
            case n of
              0 -> return Nothing
              n | c == (p-1) -> do runSTM $ writeTQueue q currentTime
                                   return $ Just (q,0,p,n-1)
                | otherwise  -> return $ Just (q,c+1,p,n)
  runSTM $ writeTVar qsS qs'

  forM_ ds $ \(q,n) ->
    untilNothing (runSTM $ tryReadTQueue q)
                 (\a -> when ((currentTime - 1 - a) > n) (deadLineMissed a (currentTime - 1) n)
                 )

  return s

stopPIO :: ComponentId -> String -> Sim ()
stopPIO cId n = notify (PeriodicIO (undefined,undefined,n)) cId PIO_Stop

deadLineMissed st et n = traceMsgTag ("Token missed deadline of " ++ show n ++ " by " ++ show (et - st - n) ++ " cycles") ("DeadlineMissed " ++ show missed)
  where
    missed = et - st - n

