module Control.Concurrent.StateMachine.TH where

import           Universum
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
    newtypeD (cxt []) (mkName typeName) [] Nothing (normalC (mkName "StateMachine") []) []

makeFsmInstance :: String -> Q Dec
makeFsmInstance typeName =
    -- instance FSM AppleGirl where
    instanceD (cxt []) (appT (conT $ mkName "Fsm") (conT $ mkName typeName))
        [ makeRunFsm typeName
        , makeReadState typeName
        ]


--runFsm logerAction initState machineDescriptione =
--    AppleGirl <$> runFsm logerAction initState machineDescriptione
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
makeReadState = error "undefined"

makeListenerInstances :: String -> [String] -> [Q Dec]
makeListenerInstances typeName eventNames =
    makeListenerInstance typeName <$> eventNames

{-
instance Listener AppleGirl Apple where
    notify        (AppleGirl fsm) = notify        fsm
    notifyAndWait (AppleGirl fsm) = notifyAndWait fsm
-}

makeListenerInstance :: String -> String -> Q Dec
makeListenerInstance = error "undefined"


{-

makeFmap :: Name -> Q Dec
makeFmap name = do
    constructors <- datatypeCons <$> reifyDatatype name
    funD (mkName "fmap") (makeFmapBody <$> constructors)

makeFmapBody :: ConstructorInfo -> Q Clause
makeFmapBody info = clause
    [varP $ mkName "g", conP consName (varP <$> varNames)]
    (normalB
        (  foldApp
        $  ConE consName
        :  (VarE <$> L.init varNames)
        ++ [UInfixE (VarE $ mkName "g") (VarE $ mkName ".") (VarE lastArg)]
        )
    )
    []
  where
    lastArg  = last varNames
    varNames = (\a -> mkName $ "a" <> show a) <$> [1 .. argNum]
    consName = constructorName info
    argNum   = length $ constructorFields info

-}

foldApp :: [Q Exp] -> Q Exp
foldApp = foldl1 appE
