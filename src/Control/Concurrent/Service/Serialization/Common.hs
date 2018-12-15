{-# Language FunctionalDependencies #-}

module Control.Concurrent.Service.Serialization.Common where

import           Control.Concurrent.Prelude

type TagType = Text

data HandlersF a b next where
    Math    :: TagType -> (b -> IO ()) -> (() -> next) -> HandlersF a b next

type HandlersL a b = Free (HandlersF a b)

class PackFormat a b msg | a -> b where
    packIn   :: a -> msg -> b

class Handlers a b m where
    handlers :: HandlersL a b () -> m ()
