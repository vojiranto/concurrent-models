module Language.Haskell.TH.Extra where

import           Universum 
import           Language.Haskell.TH

makeWraperFor :: String -> String -> Q Dec
makeWraperFor funcName typeName = funD (mkName funcName) [clause [pattern] body []]
    where
        pattern = conP (mkName typeName) [varP $ mkName "fsm"]
        body    = normalB (appE (varE $ mkName funcName) (varE $ mkName "fsm"))

foldApp :: [Q Exp] -> Q Exp
foldApp = foldl1 appE

wrap :: [Q Dec] -> Q [Dec] 
wrap decs = forM decs id
