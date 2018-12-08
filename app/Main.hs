{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
module Main where

import           Universum
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Loger
import           Control.Concurrent.Model
import           Control.Concurrent.Service.StreamController
import           Control.Concurrent.Service.Subscription

main :: IO ()
main = do
    сonsoleLogOn

    input <- streamController loger stdin 50 (const "") (Just . length)
    printer <- runActor loger $ math ((putTextLn . show) :: Int -> IO ())
    $(subscribe [t|Int|]) input printer
    forever $ threadDelay 100000000
