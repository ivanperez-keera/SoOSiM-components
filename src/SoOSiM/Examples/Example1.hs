module SoOSiM.Examples.Example1 where

import SoOSiM.Examples.Loader
import SoOSiM.Types

simstate :: IO SimState
simstate = loader "example1"
