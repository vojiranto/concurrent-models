
--{-# Language TemplateHaskell  #-}
--{-# Language QuasiQuotes      #-}

module Main where

import           Universum
import           Tcp

main :: IO ()
main = tcpExample
{-
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Loger
import           Control.Concurrent.Model
import           Control.Concurrent.Service.Stream
import           Control.Concurrent.Service.Subscription

main :: IO ()
main = do
    сonsoleLogOn
    let packageDescribe = PackegeDescribe 50 (const "") (Just . length)
    input <- makeStream loger stdin packageDescribe
    printer <- runActor loger $ math showMsg
    $(subscribe [t|Message Int|]) input printer
    forever $ threadDelay 100000000

showMsg :: Message Int -> IO ()
showMsg (Message _id i) =  putTextLn $ describe _id <> " " <> show i
-}