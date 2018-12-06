module Control.Concurrent.Model.Interface.Math where

class Math func language where
    -- | Add handler for message wich known type.
    math :: func -> language ()