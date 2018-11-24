module Control.Concurrent.StateMachine.TH
    ( makeStates
    , makeEvents
    , makeFsm
    ) where

import           Universum
import           Language.Haskell.TH.Extra 
import           Language.Haskell.TH

makeStates :: [String] -> Q [Dec]
makeStates names = forM names $ \name ->
    dataD (cxt []) (mkName name) [] Nothing [normalC (mkName name) []] []

makeEvents :: [String] -> Q [Dec]
makeEvents = makeStates

makeFsm :: String -> [String] -> Q [Dec]
makeFsm typeName eventNames = wrap
    $ makeFsmType typeName
    : makeFsmInstance typeName
    : makeListenerInstances typeName eventNames

wrap :: [Q Dec] -> Q [Dec] 
wrap decs = forM decs id

makeFsmType :: String -> Q Dec
makeFsmType typeName =
    -- newtype AppleGirl = AppleGirl StateMachine
    newtypeD (cxt []) (mkName typeName) [] Nothing (normalC (mkName "AppleGirl")
        [bangType banged (conT $ mkName "StateMachine") ]) []
    where
        banged = bang sourceNoUnpack noSourceStrictness

makeFsmInstance :: String -> Q Dec
makeFsmInstance typeName =
    -- instance FSM AppleGirl where
    instanceD (cxt []) (appT (conT $ mkName "Fsm") (conT $ mkName typeName))
        [ makeRunFsm typeName
        , makeReadState typeName
        ]

--runFsm a1 a2 a3 = AppleGirl <$> runFsm a1 a2 a3
makeRunFsm :: String -> Q Dec
makeRunFsm typeName = funD (mkName "runFsm") [clause patterns body []]
    where
        patterns = [varP $ mkName v | v <- vars]

        body     = normalB (uInfixE
            (conE $ mkName typeName)
            (varE $ mkName "<$>")
            (foldApp [varE $ mkName v | v <- "runFsm" : vars]))
        vars = ["a" <> show i | i <- [1..3 :: Int]]

-- readState (AppleGirl fsm) = readState fsm
makeReadState :: String -> Q Dec
makeReadState = makeWraperFor "readState"

makeListenerInstances :: String -> [String] -> [Q Dec]
makeListenerInstances typeName eventNames =
    makeListenerInstance typeName <$> eventNames

makeListenerInstance :: String -> String -> Q Dec
makeListenerInstance typeName msgType =
    -- instance Listener AppleGirl Apple where
    instanceD (cxt []) instanceType
        [ makeNotify        typeName
        , makeNotifyAndWait typeName
        ]
    where
        instanceType = appT
            (appT
                (conT $ mkName "Listener")
                (conT $ mkName typeName))
            (conT $ mkName msgType)

-- notify        (AppleGirl fsm) = notify        fsm
makeNotify :: String -> Q Dec
makeNotify = makeWraperFor "notify"

-- notifyAndWait (AppleGirl fsm) = notifyAndWait fsm
makeNotifyAndWait :: String -> Q Dec
makeNotifyAndWait = makeWraperFor "notifyAndWait"
