{-# Language TemplateHaskell #-}
{-# Language QuasiQuotes      #-}
module Control.Concurrent.StateMachine.TH
    ( makeStates
    , makeEvents
    , makeFsm
    ) where

import           Universum
import           Control.Concurrent.Listener
import           Language.Haskell.TH.Extra 
import           Language.Haskell.TH

makeStates :: [String] -> Q [Dec]
makeStates names = forM names $ \name ->
    dataD (cxt []) (mkName name) [] Nothing [normalC (mkName name) []] []
    

makeEvents :: [String] -> Q [Dec]
makeEvents = makeStates

makeFsm :: String -> [String] -> Q [Dec]
makeFsm typeName eventNames = wrap
    $ makeTypeWraper "StateMachine" typeName
    : makeFsmInstance typeName
    : makeListenerInstances typeName eventNames

makeFsmInstance :: String -> Q Dec
makeFsmInstance typeName =
    -- instance FSM AppleGirl where
    instanceD (cxt []) (appT (conT $ mkName "Fsm") (conT $ mkName typeName))
        [ makeMPackWraper 3 "runFsm"   typeName
        , makeUnpackWraper "readState" typeName
        ]
