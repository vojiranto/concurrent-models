module Control.Concurrent.Math where

class Math func language where
    -- | Add handler for message wich known type.
    math :: func -> language ()