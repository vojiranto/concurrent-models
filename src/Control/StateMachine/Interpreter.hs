{-# Language FlexibleInstances     #-}
{-# Language MultiParamTypeClasses #-}

module Control.StateMachine.Interpreter (makeStateMachineData) where

import           Universum
import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Lens.Getter (to)
import           Control.Loger
import           Data.Describe
import           Control.Monad.Free
import           Control.StateMachine.Language                      as L
import           Control.StateMachine.Runtime                       as R
import           Control.StateMachine.Runtime.StateMaschineStruct   as R
import           Control.StateMachine.Runtime.StateMaschineHandlers as R
import           Control.StateMachine.Domain                        as D

data BuildingError = BuildingError deriving Show

makeStateMachineData :: Loger -> D.MachineState -> L.StateMachine -> L.StateMachineL a-> IO (Either BuildingError R.StateMaschineData)
makeStateMachineData logerAction initState stateMachine h = do
    m <- newIORef $ emptyData logerAction initState 
    success <- tryAny $ foldFree (interpretStateMachineL logerAction m stateMachine) h
    mData   <- readIORef m
    pure $ case success of
        Right _  | mData ^. stateMachineStruct . to checkStruct
            -> Right mData
        _   -> Left BuildingError

interpretStateMachineL :: Loger -> IORef R.StateMaschineData -> L.StateMachine -> L.StateMachineF a -> IO a
interpretStateMachineL toLog _ _ (L.LiftIO action next) = do
    toLog "[IO action]"
    next <$> action

interpretStateMachineL toLog _ link (L.This next) = do
    toLog "[get my link]"
    pure $ next link

interpretStateMachineL toLog m _ (L.GroupStates g states next) = do
    toLog $ "[make group] " <> describe g
    modifyIORef m (R.stateMachineStruct . R.groups %~ S.insert g)
    forM_ states $ \st -> do
        toLog $ "[add states to group] " <> describe st
        mData <- readIORef m   
        -- The group tree structure is broken
        when (st `M.member` (mData ^. R.stateMachineStruct . R.groupStruct)) $
            throwM BuildExeption

        modifyIORef m (R.stateMachineStruct . R.groupStruct %~ M.insert st g)
    pure $ next ()

interpretStateMachineL toLog m _ (L.SetFinishState st next) = do
    toLog $ "[set finish state] " <> describe st
    next <$> modifyIORef m (R.stateMachineStruct . R.finishStates %~ S.insert st)

interpretStateMachineL toLog m _ (L.AddTransition st1 ev st2 next) = do
    toLog $ "[set transition] " <> describe st1 <> " -> " <> describe ev <> " -> " <> describe st2
    next <$> modifyIORef m (R.stateMachineStruct . R.transitions %~ M.insert (st1, eventToType ev) st2)    

interpretStateMachineL toLog m _ (L.AddConditionalTransition st1 ev condtition next) = do
    toLog $ "[set conditional transition] " <> describe st1 <> " -> " <> describe ev <> " -> [st] [ ? ]"
    next <$> modifyIORef m (R.stateMachineStruct . R.conditionalTransitions %~ M.insert (st1, ev) (toSafe toLog ev condtition))

interpretStateMachineL toLog m _ (L.EntryDo st action next) = do
    toLog $ "[set 'entry do' handler] " <> describe st
    next <$> modifyIORef m (R.handlers . R.entryDo %~ M.insert st (toSafeAction toLog action))

interpretStateMachineL toLog m _ (L.EntryWithEventDo st1 eventType action next) = do
    toLog $ "[set 'entry with event do' handler] " <> describe st1 <> " " <> describe eventType
    next <$> modifyIORef m (R.handlers . R.entryWithEventDo %~ M.insert (st1, eventType) (toSafe toLog eventType action))

interpretStateMachineL toLog m _ (L.StaticalDo st1 eventType action next) = do
    toLog $ "[set 'statical do' handler] " <> describe st1 <> " " <> describe eventType
    next <$> modifyIORef m (R.handlers . R.staticalDo %~ M.insert (st1, eventType) (toSafe toLog eventType  action))

interpretStateMachineL toLog m _ (L.ExitWithEventDo st1 eventType action next) = do
    toLog $ "[set 'exit with event do' handler] " <> describe st1 <> " " <> describe eventType
    next <$> modifyIORef m (R.handlers . R.exitWithEventDo %~ M.insert (st1, eventType) (toSafe toLog eventType action))    

interpretStateMachineL toLog m _ (L.ExitDo st action next) = do
    toLog $ "[set 'exit do' handler] " <> describe st
    next <$> modifyIORef m (R.handlers . R.exitDo %~ M.insert st (toSafeAction toLog action))

class ToSafe t a where
    toSafe :: (Text -> IO ()) -> t -> a -> a

instance ToSafe EventType (MachineEvent -> IO ()) where
    toSafe loger' eventType action event = catchAny (action event) $ \ex ->
        loger' $ "[error] " <> show ex <> " in action with event " <> describe eventType

instance ToSafe EventType (MachineEvent -> IO (Maybe MachineState)) where
    toSafe loger' eventType condition event = catchAny (condition event) $ \ex -> do
        loger' $ "[error] " <> show ex <> " in condition with event " <> describe eventType
        pure Nothing

toSafeAction :: (Text -> IO ()) -> IO () -> IO () 
toSafeAction loger' action = catchAny action $ \ex -> loger' $ "[error] " <> show ex <> " in action."

data BuildExeption = BuildExeption deriving Show
instance Exception BuildExeption