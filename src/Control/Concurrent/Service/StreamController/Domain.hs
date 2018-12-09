module Control.Concurrent.Service.StreamController.Domain where


import           Control.Concurrent.Prelude
--import qualified Data.Map                                                   as M


data PackegeDescribe msg = PackegeDescribe Int (msg -> ByteString) (ByteString -> Maybe msg)

getEncoder :: PackegeDescribe msg -> msg -> ByteString
getEncoder (PackegeDescribe _ encoder _) = encoder

getDecoder :: PackegeDescribe msg -> ByteString -> Maybe msg
getDecoder (PackegeDescribe _ _ decoder) = decoder

getMaxSize :: PackegeDescribe msg -> Int
getMaxSize (PackegeDescribe maxSize _ _) = maxSize
