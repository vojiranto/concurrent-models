{-# Language MultiParamTypeClasses  #-}
{-# LAnguage FunctionalDependencies #-}
module Control.Concurrent.Core.Interface.This where

import           Control.Concurrent.Prelude

class This m f | f -> m, m -> f where
    this   :: m f
    isLive :: f -> IO Bool