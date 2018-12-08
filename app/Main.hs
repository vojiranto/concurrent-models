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
    printer <- runActor loger $ math showMsg
    $(subscribe [t|Message Int|]) input printer
    forever $ threadDelay 100000000

showMsg :: Message Int -> IO ()
showMsg (Message _id i) =  putTextLn $ describe _id <> " " <> show i