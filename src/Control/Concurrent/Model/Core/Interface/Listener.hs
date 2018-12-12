{-# Language TemplateHaskell #-}
module Control.Concurrent.Model.Core.Interface.Listener where

import           Control.Concurrent.Prelude hiding (Type)
import           Language.Haskell.TH.Extra 
import           Language.Haskell.TH

class Listener a msg where
    notify        :: a -> msg -> IO ()
    notifyAndWait :: a -> msg -> IO ()

    notifyAndWait = notify

makeListenerInstances :: String -> [Q Type] -> [Q Dec]
makeListenerInstances typeName eventNames =
    makeListenerInstance typeName <$> eventNames

makeListenerInstance :: String -> Q Type -> Q Dec
makeListenerInstance typeName msgType =
    -- instance Listener AppleGirl Apple where
    instanceD (cxt []) [t|Listener $(conT $ mkName typeName) $(msgType)|]
        [ makeUnpackWraper "notify"        typeName
        , makeUnpackWraper "notifyAndWait" typeName
        ]
