{-# Language FlexibleContexts     #-}
{-# Language KindSignatures       #-}
{-# Language UndecidableInstances #-}
module Control.Concurrent.Service.Serialization.Binary where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Concurrent.Service.Serialization.Common
import           Control.Concurrent.Service.ByteStream
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
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
    packIn _ msg = B.toStrict $ encode (show $ typeOf msg :: Text, encode msg)

instance forall (m :: * -> *). (Logers m, MonadIO m, Math (Message -> IO ()) m)
    => Handlers Bin ByteString m where
    handlers _ hs = do
        loger <- getLoger
        rm    <- makeHandlers loger hs
        math $ \(Message textId byteString) -> do
            let (tag :: Text, rawMsg) = decode $ B.fromStrict byteString
            applyMHandle loger (M.lookup tag rm) tag rawMsg textId

