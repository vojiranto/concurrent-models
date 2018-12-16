{-# Language FunctionalDependencies #-}
{-# Language DeriveFunctor          #-}

module Control.Concurrent.Service.Serialization.Common where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model

type TagType = Text

data HandlersF a b next where
    Math    :: TagType -> (b -> TextId -> IO ()) -> (() -> next) -> HandlersF a b next
    deriving (Functor)


type HandlersL a b = Free (HandlersF a b)

class PackFormat a b msg | a -> b where
    packIn   :: a -> msg -> b

class Handlers a b m | a -> b where
    handlers :: a -> HandlersL a b () -> m ()
