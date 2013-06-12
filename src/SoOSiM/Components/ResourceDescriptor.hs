{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module SoOSiM.Components.ResourceDescriptor where

import Control.Applicative ((<$>),(<*>),pure)
import Data.Aeson ((.:),(.:?),(.!=),FromJSON(..),Value (..))
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import SoOSiM
import SoOSiM.Components.Common

data ISA = ANY_ISA | X86 | ARM | SPARC
  deriving (Eq,Ord,Show,Generic)

instance Hashable ISA

instance FromJSON ISA where
  parseJSON (String "x86")   = pure X86
  parseJSON (String "arm")   = pure ARM
  parseJSON (String "sparc") = pure SPARC
  parseJSON _                = pure ANY_ISA

type ResourceId = NodeId

data ResourceDescriptor
  = ANY_RES
  | ResourceDescriptor
    { isa_id   :: ISA
    , mem_size :: Int
    } deriving (Ord,Show,Generic)

instance Hashable ResourceDescriptor

instance Eq ResourceDescriptor where
  ANY_RES == _ = True
  _ == ANY_RES = True
  (ResourceDescriptor i m) == (ResourceDescriptor i' m') = i == i' && m == m'

instance FromJSON ResourceDescriptor where
  parseJSON (String "ANY_RES") = pure ANY_RES
  parseJSON (Object v) =
    ResourceDescriptor <$>
      (v .:? "isa" .!= ANY_ISA) <*>
      (v .: "mem")

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

isComplient ::
  ResourceDescriptor
  -> ResourceDescriptor
  -> Bool
isComplient ANY_RES _     = True
isComplient _   ANY_RES   = True
isComplient req available = isa_test && mem_test
  where
    mem_test = (mem_size req == 0)     || (mem_size req <= mem_size available)
    isa_test = (isa_id req == ANY_ISA) || (isa_id req == isa_id available)
