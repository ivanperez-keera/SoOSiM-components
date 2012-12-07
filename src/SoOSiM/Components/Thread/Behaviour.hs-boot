module SoOSiM.Components.Thread.Behaviour
  (TH_State,TH_Cmd,TH_Msg,threadBehaviour,threadIState)
where

import SoOSiM
import SoOSiM.Components.Thread.Types

data TH_State
data TH_Cmd
data TH_Msg

instance Typeable TH_Cmd
instance Typeable TH_Msg

threadBehaviour ::
  TH_State
  -> Input TH_Cmd
  -> Sim TH_State

threadIState :: TH_State
