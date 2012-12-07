{-# LANGUAGE TupleSections #-}
module SoOSiM.Components.ResourceManager.Behaviour where

import           Control.Arrow       (first,second)
import qualified Data.HashMap.Strict as HashMap
import           Data.List           (mapAccumR,intersect,(\\),partition)

import SoOSiM
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.ResourceManager.Interface
import SoOSiM.Components.ResourceManager.Types

resourceManager ::
  RM_State
  -> Input RM_Cmd
  -> Sim RM_State
resourceManager s (Message (AddResource rId rd) retAddr) = do
  let rs = HashMap.insert rId rd (resources s)
      s' = s { resources = rs, free_resources = rId : (free_resources s) }
  respond ResourceManager retAddr RM_Void
  yield s'

resourceManager s (Message (RequestResources appId rsList) retAddr) = do
  let res         = map (flip HashMap.filter (resources s) . isComplient . fst) rsList
      resKeys     = map HashMap.keys res
      (free',ids) = second concat $ mapAccumR checkFree (free_resources s) resKeys
      busy        = map (,appId) ids
      s'          = s { free_resources = free', busy_resources = busy ++ (busy_resources s) }
  respond ResourceManager retAddr (RM_Resources ids)
  yield s'

resourceManager s (Message (FreeResources appId) retAddr) = do
  let (freed,busy') = first (map fst) $ partition ((== appId) . snd) (busy_resources s)
      s'            = s { free_resources = freed ++ (free_resources s), busy_resources = busy' }
  respond ResourceManager retAddr RM_Void
  yield s'

resourceManager s (Message (GetResourceDescription rId) retAddr) = do
  let rdM = HashMap.lookup rId (resources s)
  respond ResourceManager retAddr (RM_Descriptor rdM)
  yield s

resourceManager s _ = yield s

checkFree :: ResourceFreeList -> [ResourceId] -> (ResourceFreeList,[ResourceId])
checkFree free keys
  = let keys' = intersect keys free
        free' = free \\ keys'
    in  (free',keys')
