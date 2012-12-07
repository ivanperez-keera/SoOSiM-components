{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module SoOSiM.Components.ProcManager.Types where

import Control.Lens

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.SoOSApplicationGraph

data PM_State
  = PM_State
  { _appId        :: AppId
  , _sched        :: ComponentId
  , _thread_graph :: ApplicationGraph
  , _rm           :: ComponentId
  }

makeLenses ''PM_State

data PM_Cmd
  = RunProgram String
  | TerminateProgram
  deriving Typeable

data PM_Msg
  = PM_Void
  deriving Typeable
