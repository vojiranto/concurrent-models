module Data.Flag (Flag, newFlag, liftFlag, wait) where

import           Universum

-- | One-time flag, for fixing the fact that an event has occurred.
newtype Flag = Flag (MVar ())

-- | Make new flag.
newFlag :: IO Flag
newFlag = Flag <$> newEmptyMVar

-- | Lift flag if it not lifted else do nothing.
liftFlag :: Flag -> IO ()
liftFlag (Flag f) = void $ tryPutMVar f ()

-- | Wait until the flag is lifted.
wait :: Flag -> IO ()
wait (Flag f) = void $ readMVar f  