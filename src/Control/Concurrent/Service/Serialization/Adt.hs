{-# Language FlexibleContexts     #-}
{-# Language KindSignatures       #-}
{-# Language UndecidableInstances #-}
module Control.Concurrent.Service.Serialization.Adt where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Concurrent.Service.Serialization.Common
import           Control.Concurrent.Service.ByteStream
import           Text.Read
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M

data Adt = Adt

instance (Typeable t, Read t) => Math (t -> TextId -> IO ()) (HandlersL Adt ByteString) where
    math action = liftF $ Math
        (show $ getTypeRepFromAction action)
        (\a textId -> action (read $ decodeUtf8 a) textId)
        id
        where
            getTypeRepFromAction :: Typeable t => (t -> TextId -> IO ()) -> TypeRep 
            getTypeRepFromAction = head . snd . splitTyConApp . typeOf

instance Show msg => PackFormat Adt ByteString msg where
    packIn _  = encodeUtf8 . (show :: msg -> Text)

instance forall (m :: * -> *). (Logers m, MonadIO m, Math (Message ByteStream -> IO ()) m)
    => Handlers Adt ByteString m where
    handlers _ hs = do
        loger <- getLoger
        rm    <- makeHandlers loger hs
        math $ \(ByteStreamMessage textId byteString) -> do
            let rawMsg = T.decodeUtf8 byteString
            let tag    = T.takeWhile (/= ' ') rawMsg
            applyMHandle loger (M.lookup tag rm) tag byteString textId
