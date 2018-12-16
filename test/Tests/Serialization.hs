{-# Language DeriveGeneric  #-}
{-# Language DeriveAnyClass #-}
module Tests.Serialization where

import           Control.Concurrent.Service.Serialization.Binary
import           Universum
import           Control.Concurrent.Model
import           Control.Concurrent.Flag
import           Control.Concurrent.Service.Stream
import           Control.Concurrent.Service.Serialization.Common
import           Data.Binary

newtype Ping = Ping Int deriving (Generic, Binary)

serializationTest :: Loger -> IO ()
serializationTest loger = do
    ok <- newFlag
    receiver <- runActor loger $
        handlers Bin $
            math $ \(Ping i) (_:: TextId) -> when (i == 2) $ liftFlag ok
    
    sender <- runActor loger $ do
        myRef <- this
        math $ \(Ping i) -> notify receiver $
            Message (getTextId myRef) (packIn Bin (Ping i)) 
    
    notify sender $ Ping 2
    wait ok