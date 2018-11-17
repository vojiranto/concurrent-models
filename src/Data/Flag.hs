module Data.Flag where

import           Universum

newtype Flag = Flag (MVar ())

newFlag :: IO Flag
newFlag = Flag <$> newEmptyMVar

liftFlag :: Flag -> IO ()
liftFlag (Flag f) = void $ tryPutMVar f ()

wait :: Flag -> IO ()
wait (Flag f) = void $ readMVar f  