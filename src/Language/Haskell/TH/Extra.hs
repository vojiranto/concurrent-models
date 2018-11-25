module Language.Haskell.TH.Extra where

import           Universum 
import           Language.Haskell.TH

-- newtype NewTypeName = NewTypeName OldTypeName
makeTypeWraper :: String -> String -> Q Dec
makeTypeWraper oldTypeName newTypeName =
    -- newtype AppleGirl = AppleGirl StateMachine
    newtypeD (cxt []) (mkName newTypeName) [] Nothing (normalC (mkName newTypeName)
        [bangType banged (conT $ mkName oldTypeName) ]) []
    where
        banged = bang sourceNoUnpack noSourceStrictness

-- funcName (TypeName a) = funcName a
makeUnpackWraper :: String -> String -> Q Dec
makeUnpackWraper funcName typeName = funD (mkName funcName) [clause [pattern] body []]
    where
        pattern = conP (mkName typeName) [varP $ mkName "a"]
        body    = normalB (appE (varE $ mkName funcName) (varE $ mkName "a"))

-- funcName a1 a2 .. an = TypeName <$> funcName a1 a2 .. an
makeMPackWraper :: Int -> String -> String -> Q Dec
makeMPackWraper varCaunter funcName typeName = funD (mkName funcName) [clause patterns body []]
    where
        patterns = [varP $ mkName v | v <- vars]

        body     = normalB (uInfixE
            (conE $ mkName typeName)
            (varE $ mkName "<$>")
            (foldApp [varE $ mkName v | v <- funcName : vars]))
        vars = ["a" <> show i | i <- [1..varCaunter]]

foldApp :: [Q Exp] -> Q Exp
foldApp = foldl1 appE

wrap :: [Q Dec] -> Q [Dec] 
wrap decs = forM decs id
