{-# Language DeriveGeneric  #-}
{-# Language DeriveAnyClass #-}
{-# Language FlexibleContexts #-}
module Tests.Serialization where

import           Universum
import           Control.Concurrent.Model
import           Control.Concurrent.Flag
import           Control.Concurrent.Service.ByteStream
import           Control.Concurrent.Service.Serialization.Common
import           Data.Binary

newtype Ping = Ping Int deriving (Generic, Binary, Read, Show)
newtype Pong = Pong Int deriving (Generic, Binary, Read, Show)

serializationTest
    :: (Math (Pong -> TextId -> IO ()) (HandlersL a b),
        Math (Ping -> TextId -> IO ()) (HandlersL a b),
        Handlers a b ActorL,
        PackFormat a ByteString Ping, PackFormat a ByteString Pong) =>
        a -> Loger -> IO ()
serializationTest format loger = do
    okPing <- newFlag
    okPong <- newFlag
    receiver <- runActor loger $
        handlers format $ do
            math $ \(Ping i) (_:: TextId) -> when (i == 2) $ liftFlag okPing
            math $ \(Pong i) (_:: TextId) -> when (i == 12) $ liftFlag okPong
    
    void $ runActor loger $ do
        myRef <- this
        liftIO $ do
            notify receiver $ ByteStreamMessage (getTextId myRef) (packIn format (Ping 2))
            notify receiver $ ByteStreamMessage (getTextId myRef) (packIn format (Pong 12)) 
            stopRole myRef

    wait okPing
    wait okPong