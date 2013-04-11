module SoOSiM.Components.MemoryManager.Behaviour where

import SoOSiM
import SoOSiM.Components.MemoryManager.Types

memMgr :: MM_State
       -> Input MM_Cmd
       -> Sim MM_State
