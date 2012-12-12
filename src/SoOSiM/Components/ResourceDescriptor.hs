{-# LANGUAGE OverloadedStrings #-}
module SoOSiM.Components.ResourceDescriptor where

import Control.Applicative ((<$>),(<*>),pure)
import Data.Aeson ((.:),FromJSON(..),Value (..))

import SoOSiM
import SoOSiM.Components.Common

data ISA = ANY_ISA | X86 | ARM | SPARC
  deriving (Eq,Ord,Show)

type ResourceId = NodeId

data ResourceDescriptor
  = ResourceDescriptor
  { isa_id   :: ISA
  , mem_size :: Int
  } deriving (Eq,Ord,Show)

instance FromJSON ResourceDescriptor where
  parseJSON (String "ANY_RES") = pure anyRes

data Resource
  = Resource
  { res_id   :: ResourceId
  , res_type :: ResourceDescriptor
  }

instance FromJSON Resource where
  parseJSON (Object v) =
    Resource <$>
      (v .: "id") <*>
      (v .: "type")

anyRes = ResourceDescriptor ANY_ISA 0

isComplient ::
  ResourceDescriptor
  -> ResourceDescriptor
  -> Bool
isComplient req available = isa_test && mem_test
  where
    mem_test = (mem_size req == 0)     || (mem_size req <= mem_size available)
    isa_test = (isa_id req == ANY_ISA) || (isa_id req == isa_id available)
