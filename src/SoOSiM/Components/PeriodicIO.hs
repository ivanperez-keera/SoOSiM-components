{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.PeriodicIO where

import Control.Concurrent.STM.TQueue
import Control.Monad
import Data.Maybe

import SoOSiM

newtype PeriodicIO = PeriodicIO ([(TQueue Int,Int,Int,Int)],[(TQueue Int,Int)],String)

data PIO_Cmd = PIO_Stop
  deriving Typeable

instance ComponentInterface PeriodicIO where
  type State PeriodicIO               = PeriodicIO
  type Receive PeriodicIO             = PIO_Cmd
  type Send PeriodicIO                = ()
  initState                           = const (PeriodicIO ([],[],""))
  componentName (PeriodicIO (_,_,an)) = ("<<" ++ an ++ ">>Periodic IO")
  componentBehaviour                  = const periodicIO

periodicIO ::
  PeriodicIO
  -> Input PIO_Cmd
  -> Sim PeriodicIO
periodicIO (PeriodicIO (_,_,_)) (Message _ PIO_Stop _) = stop

periodicIO (PeriodicIO (qs,ds,n)) _ = do
  currentTime <- getTime
  qs' <- fmap catMaybes $ forM qs $ \(q,c,p,n) -> do
            case n of
              0 -> return Nothing
              n | c == (p-1) -> do runSTM $ writeTQueue q currentTime
                                   return $ Just (q,0,p,n-1)
                | otherwise  -> return $ Just (q,c+1,p,n)
  return (PeriodicIO (qs',ds,n))

stopPIO :: ComponentId -> String -> Sim ()
stopPIO cId n = notify (PeriodicIO (undefined,undefined,n)) cId PIO_Stop
