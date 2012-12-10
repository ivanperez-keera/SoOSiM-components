module SoOSiM.Components.ApplicationHandler.Behaviour where

import SoOSiM
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.SoOSApplicationGraph

import SoOSiM.Components.ApplicationHandler.Interface
import SoOSiM.Components.ApplicationHandler.Types

appHandler ::
  AH_State
  -> Input AH_Cmd
  -> Sim AH_State
appHandler s (Message (LoadProgram "example1") retAddr) = do
  respond ApplicationHandler retAddr (AH_AG example1)
  yield s

appHandler s (Message (LoadProgram "example2") retAddr) = do
  respond ApplicationHandler retAddr (AH_AG example1)
  yield s

appHandler s _ = yield s

example1 :: ApplicationGraph
example1 = ApplicationGraph v e
  where
    v = [ Vertex 1 anyRes 0 1
        , Vertex 2 anyRes 0 1
        , Vertex 3 anyRes 0 1
        , Vertex 4 anyRes 0 1
        ]

    e = [ Edge 1 2 4
        , Edge 1 3 0
        , Edge 2 4 0
        , Edge 3 4 0
        ]

example2 :: ApplicationGraph
example2 = ApplicationGraph v e
  where
    v = [ Vertex 1 anyRes 0 1
        , Vertex 2 anyRes 0 3
        , Vertex 3 anyRes 0 1
        , Vertex 4 anyRes 0 1
        ]

    e = [ Edge 1 2 4
        , Edge 1 3 0
        , Edge 2 4 0
        , Edge 3 4 0
        ]
