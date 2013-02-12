{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.PeriodicIO where

import Control.Concurrent.STM.TQueue
import Control.Monad
import Data.Maybe

import SoOSiM

newtype PeriodicIO = PeriodicIO ([(TQueue Int,Int,Int,Int)],String)

instance ComponentInterface PeriodicIO where
  type State PeriodicIO             = PeriodicIO
  type Receive PeriodicIO           = ()
  type Send PeriodicIO              = ()
  initState                         = const (PeriodicIO ([],""))
  componentName (PeriodicIO (_,an)) = ("<<" ++ an ++ ">>Periodic IO")
  componentBehaviour                = const periodicIO

periodicIO ::
  PeriodicIO
  -> Input ()
  -> Sim PeriodicIO
periodicIO (PeriodicIO ([],_)) _ = stop

periodicIO (PeriodicIO (qs,n)) _ = do
  currentTime <- getTime
  qs' <- fmap catMaybes $ forM qs $ \(q,c,p,n) -> do
            case n of
              0 -> return Nothing
              n | c == (p-1) -> do runSTM $ writeTQueue q currentTime
                                   return $ Just (q,0,p,n-1)
                | otherwise  -> return $ Just (q,c+1,p,n)
  return (PeriodicIO (qs',n))
