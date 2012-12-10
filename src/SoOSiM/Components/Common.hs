{-# LANGUAGE Rank2Types #-}
module SoOSiM.Components.Common where

import Data.Maybe
import Control.Monad
import SoOSiM

type GUID = Int

type AppId     = GUID
type ProcessId = ComponentId
type ThreadId  = ProcessId

data Code                = Code

data Architecture        = Architecture
data ResourceDescription = ResourceDescription

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m a f = maybe a f m

whenM :: Monad m => (m Bool) -> m () -> m ()
whenM t f = do
  t >>= (\t' -> when t' f)

uncurry2 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry2 f (a,b,c) = f a b c
