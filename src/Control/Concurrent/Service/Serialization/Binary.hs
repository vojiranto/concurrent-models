{-# Language FlexibleContexts     #-}
{-# Language KindSignatures       #-}
{-# Language UndecidableInstances #-}
module Control.Concurrent.Service.Serialization.Binary where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Concurrent.Service.Serialization.Common
import           Control.Concurrent.Service.Stream
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
        rm <- liftIO $ do
            rm <- newIORef mempty
            void $ foldFree (makeHandlers loger rm) hs
            readIORef rm
        math $ \(Message textId byteString) -> case decode $ B.fromStrict byteString of
            (tag :: Text, rawMsg) -> case M.lookup tag rm of
                Just handle -> do
                    loger Trace $ "Accepted message " <> tag <> " from " <> describe textId
                    handle rawMsg textId
                _   -> loger Warn $ "Handle for msg  " <> tag <> " not exixs."
    
makeHandlers :: Loger -> IORef (Map Text (ByteString -> TextId -> IO ())) -> HandlersF Bin ByteString m -> IO m
makeHandlers toLoger m (Math eventType' action next) = do
    dataStruct <- readIORef m
    let logTail = eventType'
    if M.member eventType' dataStruct
        then toLoger Warn  $ "[handler 'math' already exists] " <> logTail
        else toLoger Trace $ "[set 'math' handler] " <> logTail
    next <$> modifyIORef m (M.insert eventType' action)
    