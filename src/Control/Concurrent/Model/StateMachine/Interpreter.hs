{-# Language FlexibleInstances     #-}
{-# Language MultiParamTypeClasses #-}

module Control.Concurrent.Model.StateMachine.Interpreter (makeStateMachineData) where

import           Control.Concurrent.Prelude
import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Concurrent.Model.Core
import           Control.Concurrent.Model.StateMachine.Language             as L
import           Control.Concurrent.Model.StateMachine.Runtime              as R
import           Control.Concurrent.Model.StateMachine.Domain               as D

data BuildingError = BuildingError deriving Show

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

interpretStateMachineL :: Loger -> IORef R.StateMaschineData -> L.StateMachine -> L.StateMachineF a -> IO a
interpretStateMachineL toLog' _ _ (L.LiftIO action next) = do
    toLog' Trace "[IO action]"
    next <$> action

interpretStateMachineL toLog' _ _ (L.GetLoger  next) = do
    toLog' Trace "[get loger]"
    pure $ next toLog'

interpretStateMachineL toLog' _ _ (L.ToLog logLevel text next) = do
    toLog' logLevel text
    pure $ next ()

interpretStateMachineL toLog' _ link (L.This next) = do
    toLog' Trace "[get this *]"
    pure $ next link

interpretStateMachineL toLog' m _ (L.GroupStates g states next) = do
    toLog' Trace $ "[make group] " <> describe g
    modifyIORef m (R.stateMachineStruct . R.groups %~ S.insert g)
    forM_ states $ \st -> do
        toLog' Trace $ "[add states to group] " <> describe st
        mData <- readIORef m   
        -- The group tree structure is broken
        when (st `M.member` (mData ^. R.stateMachineStruct . R.groupStruct)) $
            throwM BuildExeption

        modifyIORef m (R.stateMachineStruct . R.groupStruct %~ M.insert st g)
    pure $ next ()

interpretStateMachineL toLog' m _ (L.AddFinalState st next) = do
    toLog' Trace $ "[set finish state] " <> describe st
    next <$> modifyIORef m (R.stateMachineStruct . R.finishStates %~ S.insert st)

interpretStateMachineL toLog' m _ (L.AddTransition st1 ev st2 next) = do
    dataStruct <- readIORef m
    let key     = (st1, eventToType ev)
    let logTail = describe st1 <> " -> " <> describe ev <> " -> " <> describe st2
    if M.member key (dataStruct ^. R.stateMachineStruct . R.transitions)
        then toLog' Warn  $ "[the transition already exists] " <> logTail
        else toLog' Trace $ "[set transition] " <> logTail 
    next <$> modifyIORef m (R.stateMachineStruct . R.transitions %~ M.insert key st2)    

interpretStateMachineL toLog' m _ (L.AddConditionalTransition st1 ev condtition next) = do
    dataStruct <- readIORef m
    let key     = (st1, ev)
    let logTail = describe st1 <> " -> " <> describe ev <> " -> [state ? ]"
    if M.member key (dataStruct ^. R.stateMachineStruct . R.conditionalTransitions)
        then toLog' Warn  $ "[the conditional transition already exists] " <> logTail
        else toLog' Trace $ "[set conditional transition] " <> logTail
    next <$> modifyIORef m (R.stateMachineStruct . R.conditionalTransitions %~ M.insert key (toSafe toLog' ev condtition))

interpretStateMachineL toLog' m _ (L.MathDo eventType' action next) = do
    dataStruct <- readIORef m
    let logTail = describe eventType'
    if M.member eventType' (dataStruct ^. R.handlers . R.mathDo)
        then toLog' Warn  $ "[handler 'math' already exists] " <> logTail
        else toLog' Trace $ "[set 'math' handler] " <> logTail
    next <$> modifyIORef m (R.handlers . R.mathDo %~ M.insert eventType' (toSafe toLog' eventType' action))

interpretStateMachineL toLog' m _ (L.EntryDo st action next) = do
    dataStruct <- readIORef m
    let logTail = describe st
    if M.member st (dataStruct ^. R.handlers . R.entryDo)
        then toLog' Warn $ "[handler 'entry do' already exists] " <> logTail
        else toLog' Trace $ "[set 'entry do' handler] " <> logTail
    next <$> modifyIORef m (R.handlers . R.entryDo %~ M.insert st (toSafeAction toLog' action))

interpretStateMachineL toLog' m _ (L.EntryWithEventDo st1 eventType' action next) = do
    dataStruct <- readIORef m
    let logTail = describe st1 <> " " <> describe eventType'
    if M.member (st1, eventType') (dataStruct ^. R.handlers . R.entryWithEventDo)
        then toLog' Warn $ "[handler 'entry do' already exists] " <> logTail
        else toLog' Trace $ "[set 'entry do' handler] " <> logTail
    next <$> modifyIORef m (R.handlers . R.entryWithEventDo %~ M.insert (st1, eventType') (toSafe toLog' eventType' action))

interpretStateMachineL toLog' m _ (L.StaticalDo st1 eventType' action next) = do
    dataStruct <- readIORef m
    let logTail = describe st1 <> " " <> describe eventType'
    if M.member (st1, eventType') (dataStruct ^. R.handlers . R.staticalDo)
        then toLog' Warn $ "[handler 'math' already exists] " <> logTail
        else toLog' Trace $ "[set 'math' handler] " <> logTail
    next <$> modifyIORef m (R.handlers . R.staticalDo %~ M.insert (st1, eventType') (toSafe toLog' eventType'  action))

interpretStateMachineL toLog' m _ (L.ExitWithEventDo st1 eventType' action next) = do
    -- TODO: add warning if handler already exists.
    dataStruct <- readIORef m
    let logTail = describe st1 <> " " <> describe eventType'
    if M.member (st1, eventType') (dataStruct ^. R.handlers . R.exitWithEventDo)
        then toLog' Warn $ "[handler 'exit do' already exists] " <> logTail
        else toLog' Trace $ "[set 'exit do' handler] " <> logTail
    next <$> modifyIORef m (R.handlers . R.exitWithEventDo %~ M.insert (st1, eventType') (toSafe toLog' eventType' action))    

interpretStateMachineL toLog' m _ (L.ExitDo st action next) = do
    dataStruct <- readIORef m
    let logTail = describe st
    if M.member st (dataStruct ^. R.handlers . R.exitDo)
        then toLog' Warn $ "[handler 'exit do' already exists] " <> logTail
        else toLog' Trace $ "[set 'exit do' handler] " <> logTail
    next <$> modifyIORef m (R.handlers . R.exitDo %~ M.insert st (toSafeAction toLog' action))

class ToSafe t a where
    toSafe :: Loger -> t -> a -> a

instance ToSafe EventType (Event -> IO ()) where
    toSafe loger' eventType' action event = catchAny (action event) $ \ex ->
        loger' Warn $ show ex <> " in action with event " <> describe eventType'

instance ToSafe EventType (Event -> IO (Maybe MachineState)) where
    toSafe loger' eventType' condition event = catchAny (condition event) $ \ex -> do
        loger' Warn $ show ex <> " in condition with event " <> describe eventType'
        pure Nothing

toSafeAction :: Loger -> IO () -> IO () 
toSafeAction loger' action = catchAny action $ \ex -> loger' Warn $ show ex <> " in action."

data BuildExeption = BuildExeption deriving Show
instance Exception BuildExeption