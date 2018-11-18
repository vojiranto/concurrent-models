
# Actor

What can actors do? They can: receive messages, send messages, create new actors and change their internal state.

```haskell
module Actor.PingPong where

import           Universum

import           Data.Flag        -- To report about successful completion.
import           Control.Loger
import           Control.Actor

-- You can send different types of messages to actors.
-- If there is no suitable handler, then it will simply be droped.
data Ping = Ping Actor Int
data Pong = Pong Actor Int

actorPingPong :: IO ()
actorPingPong = do
    success <- newFlag
    actor1  <- makeActor logOff $ \link -> do
        math $ ping success link
        math $ pong success link

    actor2  <- makeActor logOff $ \link -> do
        math $ ping success link
        math $ pong success link

    notify actor2 $ Ping actor1 10
    wait success

-- Handlers are ordinary Haskel functions.
ping :: Flag -> Actor -> Ping -> IO ()
ping success _    (Ping _     0) = liftFlag success
ping _       link (Ping actor n) = notify actor $ Pong link (n-1)

pong :: Flag -> Actor -> Pong -> IO ()
pong success _    (Pong _     0) = liftFlag success
pong _       link (Pong actor n) = notify actor $ Ping link (n-1)

```

# FSM

FSM is framework to build asynchronous FSM. What is supported?

1. Initial state, final state, transitions by events.
2. Dynamic selection of transitions by event and current state.
3. State grouping and grouping of groups.
4. Addition transition from goup.
5. Handlers state and groups entry/exit.
6. Handlers for events.
7. Log of transitions, events and internal errors (You can direct it wherever you want.)
8. Functions for working with internal types (emit, emitAndWait, is, <:, nothing, just...).

```haskell
module StateMachine.TrafficLight where

import           Universum
import           Control.Loger
import           Control.StateMachine

-- states for traffic light
data Green       = Green
data Yellow      = Yellow
data Red         = Red

-- event
data ChangeColor = ChangeColor

makeTrafficLight :: IO StateMachine
makeTrafficLight = runStateMachine logToConsole Green $ do
    -- add to fsm transitions from one states to another.
    addTransition Green  ChangeColor Yellow
    addTransition Red    ChangeColor Yellow

    -- during the construction of the FSN, you can use IO.
    directionSM <- liftIO $ runStateMachine logToConsole Red $ do
        addTransition Green  ChangeColor Red
        addTransition Red    ChangeColor Green

    exitDo Red   $ emitAndWait directionSM ChangeColor
    exitDo Green $ emitAndWait directionSM ChangeColor

    -- add context dependent transition
    -- naturally, you can also use logical conditions for events.
    addConditionalTransition Yellow $
        \ChangeColor -> Just <$> takeState directionSM

-- read lines have not yet read the "exit"
readLightCommand :: StateMachine -> IO ()
readLightCommand sm = do
    line <- getLine
    when (line /= "exit") $ do
        emit sm ChangeColor
        readLightCommand sm

```
