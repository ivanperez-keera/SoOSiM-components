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

untilJust ::
  Monad m
  => (m (Maybe a))
  -> m a
untilJust mf = do
  aM <- mf
  case aM of
    Just a  -> return a
    Nothing -> untilJust mf

untilNothing ::
  Monad m
  => m (Maybe a)
  -> (a -> m ())
  -> m ()
untilNothing mf mu = do
  aM <- mf
  case aM of
    Nothing -> return ()
    Just a  -> mu a >> untilNothing mf mu

(><) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(f >< g) (a,b) = (f a, g b)

dot = (.) . (.)
