{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.ApplicationHandler.Types where

import SoOSiM
import SoOSiM.Components.SoOSApplicationGraph

data AH_State
  = AH_State

data AH_Cmd
  = LoadProgram String
  deriving Typeable

data AH_Msg
  = AH_AG ApplicationGraph
  deriving Typeable
