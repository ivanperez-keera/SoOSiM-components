{-# LANGUAGE OverloadedStrings #-}
module SoOSiM.Examples.Parser where

import Data.Aeson           ((.:),eitherDecode,FromJSON(..),Value (..))
import Data.ByteString.Lazy as BS
import Data.Maybe           (fromJust)
import Control.Applicative  ((<$>),(<*>))

import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.SoOSApplicationGraph

data Example
  = Example [ApplicationGraph] String [Resource]

instance FromJSON Example where
  parseJSON (Object v) =
    Example <$>
      (v .: "Apps") <*>
      (v .: "Distribution") <*>
      (v .: "Platform")

readExample ::
  FilePath
  -> IO Example
readExample fn = do
  exampleBS <- BS.readFile fn
  let example = either error id $ eitherDecode exampleBS
  return $! example
