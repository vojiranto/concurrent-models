{-# LANGUAGE TemplateHaskell #-}
module Actor.TypeSafe where

import           Universum
import           Control.Concurrent.Model

data Msg = Msg

makeAct "SafeAct" ["Msg"]
