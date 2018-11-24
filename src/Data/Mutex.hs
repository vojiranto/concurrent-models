module Data.Mutex where

import           Universum

newtype Mutex = Mutex (MVar ())

newMutex :: IO Mutex
newMutex = Mutex <$> newMVar ()

putMutex :: Mutex -> IO ()
putMutex (Mutex mutex) = putMVar mutex ()

takeMutex :: Mutex -> IO ()
takeMutex (Mutex mutex) = takeMVar mutex
