module Control.Concurrent.Model.Core.Interface.Listener where

import           Control.Concurrent.Prelude
import           Language.Haskell.TH.Extra 
import           Language.Haskell.TH

class Listener a msg where
    notify        :: a -> msg -> IO ()
    notifyAndWait :: a -> msg -> IO ()

    notifyAndWait = notify

makeListenerInstances :: String -> [String] -> [Q Dec]
makeListenerInstances typeName eventNames =
    makeListenerInstance typeName <$> eventNames

makeListenerInstance :: String -> String -> Q Dec
makeListenerInstance typeName msgType =
    -- instance Listener AppleGirl Apple where
    instanceD (cxt []) instanceType
        [ makeUnpackWraper "notify"        typeName
        , makeUnpackWraper "notifyAndWait" typeName
        ]
    where
        instanceType = appT
            (appT
                (conT $ mkName "Listener")
                (conT $ mkName typeName))
            (conT $ mkName msgType)