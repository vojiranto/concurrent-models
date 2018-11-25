module Control.Concurrent.Actor.TH (makeAct) where

import           Universum
import           Control.Concurrent.Listener
import           Language.Haskell.TH.Extra 
import           Language.Haskell.TH

makeAct :: String -> [String] -> Q [Dec]
makeAct typeName eventNames = wrap
    $ makeTypeWraper "Actor" typeName
    : makeActInstance typeName
    : makeListenerInstances typeName eventNames

makeActInstance :: String -> Q Dec
makeActInstance typeName =
    -- instance FSM AppleGirl where
    instanceD (cxt []) (appT (conT $ mkName "Role") (conT $ mkName typeName))
        [ makeMPackWraper 2 "runRole"   typeName
        , makeUnpackWraper "stopRole" typeName
        , makeUnpackWraper "killRole" typeName
        ]
