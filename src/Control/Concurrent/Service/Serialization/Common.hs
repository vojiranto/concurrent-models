{-# Language FunctionalDependencies #-}
{-# Language DeriveFunctor          #-}

module Control.Concurrent.Service.Serialization.Common where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import qualified Data.Map as M

type TagType = Text

data HandlersF a b next where
    Math    :: TagType -> (b -> TextId -> IO ()) -> (() -> next) -> HandlersF a b next
    deriving (Functor)


type HandlersL a b = Free (HandlersF a b)

class PackFormat a b msg | a -> b where
    packIn   :: a -> msg -> b

class Handlers a b m | a -> b where
    handlers :: a -> HandlersL a b () -> m ()

makeHandlers
    :: MonadIO m
    => Loger -> HandlersL f ByteString a -> m (Map Text (ByteString -> TextId -> IO ()))
makeHandlers loger hs = liftIO $ do
    rm <- newIORef mempty
    void $ foldFree (makeHandlersMap loger rm) hs
    readIORef rm

makeHandlersMap :: Loger -> IORef (Map Text (ByteString -> TextId -> IO ())) -> HandlersF f ByteString m -> IO m
makeHandlersMap toLoger m (Math eventType' action next) = do
    dataStruct <- readIORef m
    let logTail = eventType'
    if M.member eventType' dataStruct
        then toLoger Warn  $ "[handler 'math' already exists] " <> logTail
        else toLoger Trace $ "[set 'math' handler] " <> logTail
    next <$> modifyIORef m (M.insert eventType' action)

applyMHandle
    :: Loger -> Maybe (t2 -> TextId -> IO ()) -> Text -> t2 -> TextId -> IO ()
applyMHandle loger mHandle tag rawMsg textId = case mHandle of
    Just handle -> do
        loger Trace $ "Accepted message " <> tag <> " from " <> describe textId
        handle rawMsg textId
    _   -> loger Warn $ "Handle for msg  " <> tag <> " not exixs."
