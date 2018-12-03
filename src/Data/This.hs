{-# Language MultiParamTypeClasses  #-}
{-# LAnguage FunctionalDependencies #-}
module Data.This where

import           Universum

class This m f | f -> m, m -> f where
    this   :: m f
    isLive :: f -> IO Bool