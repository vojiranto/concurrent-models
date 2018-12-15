
module Control.Concurrent.Service.Serialization.Binary where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Concurrent.Service.Serialization.Common
import qualified Data.ByteString.Lazy as B
import           Data.Binary


data Bin = Bin

instance (Typeable t, Binary t) => Math (t -> TextId -> IO ()) (HandlersL Bin ByteString) where
    math action = liftF $ Math
        (show $ getTypeRepFromAction action)
        (\a textId -> action (decode $ B.fromStrict a) textId)
        id
        where
            getTypeRepFromAction :: Typeable t => (t -> TextId -> IO ()) -> TypeRep 
            getTypeRepFromAction = head . snd . splitTyConApp . typeOf

instance (Typeable msg, Binary msg) => PackFormat Bin ByteString msg where
    packIn _ msg = B.toStrict $ encode (show $ typeOf msg :: Text, msg)