{-# LANGUAGE TemplateHaskell #-}
module Actor.TypeSafe where

import           Universum
import           Control.Concurrent.Model
import           Control.Concurrent.Actor

data Msg = Msg

makeAct "SafeAct" ["Msg"]
