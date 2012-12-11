module SoOSiM.Components.SoOSApplicationGraph where

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor

type VertexId = Int

data Vertex
  = Vertex
  { v_id                  :: VertexId
  , resourceRequirements  :: ResourceDescriptor
  , pointerToCodeInMemory :: Int
  , executionTime         :: Int
  } deriving Show

data Edge
  = Edge
  { start    :: VertexId
  , end      :: VertexId
  , n_tokens :: Int
  } deriving Show

data ApplicationGraph
  = ApplicationGraph
  { vertices :: [Vertex]
  , edges    :: [Edge]
  } deriving Show

numberOfVertices :: ApplicationGraph -> Int
numberOfVertices (ApplicationGraph v _) = length v
