{-# LANGUAGE OverloadedStrings #-}
module SoOSiM.Examples.Parser where

import Data.Aeson           ((.:),decode,FromJSON(..),Value (..))
import Data.ByteString.Lazy as BS
import Data.Maybe           (fromJust)
import Control.Applicative  ((<$>),(<*>))

import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.SoOSApplicationGraph

data Example
  = Example [ApplicationGraph] [Resource]

instance FromJSON Example where
  parseJSON (Object v) =
    Example <$>
      (v .: "Apps") <*>
      (v .: "Platform")

readExample ::
  FilePath
  -> IO Example
readExample fn = do
  exampleBS <- BS.readFile fn
  let example = fromJust $ decode exampleBS
  return $! example
