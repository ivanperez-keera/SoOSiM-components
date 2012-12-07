{-# LANGUAGE Rank2Types #-}
module SoOSiM.Components.Common where

import Data.Maybe
import Control.Lens
import Control.Monad.State.Strict
import SoOSiM

type GUID = Int

type AppId     = GUID
type ProcessId = ComponentId
type ThreadId  = ProcessId

data Code                = Code

data Architecture        = Architecture
data ResourceDescription = ResourceDescription

getTime :: Sim Int
getTime = undefined

maybe' m a f = maybe a f m

whenM :: Monad m => (m Bool) -> m () -> m ()
whenM t f = do
  t >>= (\t' -> when t' f)

uncurry2 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry2 f (a,b,c) = f a b c

(%=~) :: MonadState s m => Lens s s a b -> (a -> m b) -> m ()
l %=~ f = use l >>= f >>= assign l
