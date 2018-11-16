module Control.StateMachine.TH where

import           Universum
import           Language.Haskell.TH

makeStates :: [String] -> Q [Dec]
makeStates names = forM names $ \name ->
    dataD (cxt []) (mkName name) [] Nothing [normalC (mkName name) []] []

makeEvents :: [String] -> Q [Dec]
makeEvents = makeStates