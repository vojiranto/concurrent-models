module Control.Concurrent.Listener where

import           Universum

class Listener a msg where
    notify        :: a -> msg -> IO ()
    notifyAndWait :: a -> msg -> IO ()

    notifyAndWait = notify