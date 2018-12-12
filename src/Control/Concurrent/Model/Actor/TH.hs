module Control.Concurrent.Model.Actor.TH (makeAct) where

import           Universum hiding (Type)
import           Control.Concurrent.Model.Core.Interface.Listener
import           Language.Haskell.TH.Extra 
import           Language.Haskell.TH

makeAct :: String -> [Q Type] -> Q [Dec]
makeAct typeName eventNames = wrap
    $ makeTypeWraper "Actor" typeName
    : makeActInstance typeName
    : makeListenerInstances typeName eventNames

makeActInstance :: String -> Q Dec
makeActInstance typeName =
    -- instance Role AppleGirl where
    instanceD (cxt []) (appT (conT $ mkName "Role") (conT $ mkName typeName))
        [ makeMPackWraper 2 "runRole"   typeName
        , makeUnpackWraper "stopRole" typeName
        , makeUnpackWraper "killRole" typeName
        ]
