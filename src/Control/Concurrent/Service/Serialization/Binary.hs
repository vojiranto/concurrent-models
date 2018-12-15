{-# Language FlexibleContexts #-}
{-# Language UndecidableInstances #-}
module Control.Concurrent.Service.Serialization.Binary where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Concurrent.Service.Serialization.Common
--import           Control.Concurrent.Service.Stream
import qualified Data.ByteString.Lazy as B
--import qualified Data.Map as M
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
{-
instance (Logers m, Math (Message -> IO ()) m) => Handlers Bin ByteString m where
    handlers hs = do
        loger <- getLoger
        rm <- liftIO $ do
            rm <- newIORef mempty
            foldFree (makeHandlers loger rm) hs
            pure rm
        math $ error ""

makeHandlers :: Loger -> IORef (Map Text (ByteString -> TextId -> IO ())) -> HandlersF Bin ByteString m -> IO ()
makeHandlers toLoger m (Math eventType' action next) = do
    dataStruct <- readIORef m
    let logTail = describe eventType'
    if M.member eventType' dataStruct
        then toLoger Warn  $ "[handler 'math' already exists] " <> logTail
        else toLoger Trace $ "[set 'math' handler] " <> logTail
    next <$> modifyIORef m (%~ M.insert eventType' (toSafe toLoger eventType' action))
    

toSafe :: Loger -> Text -> (ByteString -> TextId -> IO ()) -> ByteString -> TextId -> IO ()
toSafe loger messageType action message textId = catchAny (action message textId) $ \ex ->
    loger Warn $ show ex <> " in action with message " <> messageType
-}