{-# Language MultiParamTypeClasses  #-}
{-# LAnguage FunctionalDependencies #-}
module Data.This where

class This m f | f -> m, m -> f where
    this :: m f