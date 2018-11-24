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

{-
-- makeFsm "AppleGirl" ["Apple"]
newtype AppleGirl = AppleGirl StateMachine
-}
makeFsmType :: String -> Q Dec
makeFsmType = error "undefined"
{-
instance FSM AppleGirl where
    runFsm logerAction initState machineDescriptione =
        AppleGirl <$> runFsm logerAction initState machineDescriptione

    readState (AppleGirl fsm) = readState fsm
-}
makeFsmInstance :: String -> Q Dec
makeFsmInstance = error "undefined"

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
module Language.Haskell.TH.MakeFunctor where

import           Enecuum.Prelude
-- import           Control.Monad
import qualified Data.List as L
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype

makeFunctorInstance :: Name -> Q [Dec]
makeFunctorInstance name =
    forM [1 :: Int] $ \_ -> instanceD (cxt []) (appT (conT $ mkName "Functor") (conT name)) [makeFmap name]

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

foldApp :: [Exp] -> Q Exp
foldApp = pure . foldl1 AppE
-}