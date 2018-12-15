
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


{-
interpretStateMachineL toLog' m _ (L.MathDo eventType' action next) = do
    dataStruct <- readIORef m
    let logTail = describe eventType'
    if M.member eventType' (dataStruct ^. R.handlers . R.mathDo)
        then toLog' Warn  $ "[handler 'math' already exists] " <> logTail
        else toLog' Trace $ "[set 'math' handler] " <> logTail
    next <$> modifyIORef m (R.handlers . R.mathDo %~ M.insert eventType' (toSafe toLog' eventType' action))

makeStateMachineData
    :: Loger
    -> L.StateMachine
    -> L.StateMachineL a
    -> IO (Either BuildingError (IORef R.StateMaschineData))
makeStateMachineData logerAction stateMachine h = do
    initState <- takeState stateMachine
    m <- newIORef $ emptyData logerAction initState 
    success <- tryAny $ foldFree (interpretStateMachineL logerAction m stateMachine) h
    mData   <- readIORef m
    case success of
        Right _  | mData ^. stateMachineStruct . to checkStruct ->
            Right <$> newIORef mData
        _   -> pure $ Left BuildingError

-}