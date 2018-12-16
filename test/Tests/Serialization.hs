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
newtype Pong = Pong Int deriving (Generic, Binary)

serializationTest :: Loger -> IO ()
serializationTest loger = do
    okPing <- newFlag
    okPong <- newFlag
    receiver <- runActor loger $
        handlers Bin $ do
            math $ \(Ping i) (_:: TextId) -> when (i == 2) $ liftFlag okPing
            math $ \(Pong i) (_:: TextId) -> when (i == 12) $ liftFlag okPong
    
    void $ runActor loger $ do
        myRef <- this
        liftIO $ do
            notify receiver $ Message (getTextId myRef) (packIn Bin (Ping 2))
            notify receiver $ Message (getTextId myRef) (packIn Bin (Pong 12)) 
            stopRole myRef

    wait okPing
    wait okPong