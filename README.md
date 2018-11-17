# Actor
What can the actors? Actors can: receive messages, send messages, create new actors, and change the internal state.

```haskell
import           Universum

import           Data.Flag      -- To report about successful completion.
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

-- Handlers are ordinary haskel functions with strong typing.
-- Thanks to io you can enjoy all the wealth of opportunities.
-- For example shoot yourself in the foot ;)
ping :: Flag -> Actor -> Ping -> IO ()
ping sem  _    (Ping _     0) = liftFlag sem
ping _    link (Ping actor n) = notify actor $ Pong link (n-1)

pong :: Flag -> Actor -> Pong -> IO ()
pong sem  _    (Pong _     0) = liftFlag sem
pong _    link (Pong actor n) = notify actor $ Ping link (n-1)

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
import           Universum

import           Control.Loger
import           Control.StateMachine
import           Control.StateMachine.Domain

makeStates ["Green", "Yellow", "Red"]
makeEvents ["ChangeColor"]

trafficLightExample :: IO StateMachine
trafficLightExample = runStateMachine logToConsole Green $ do
    addTransition Green  ChangeColor Yellow
    addTransition Red    ChangeColor Yellow
    
    -- during the construction of the FSN, you can use IO.
    directionSM <- initialiseAction $ runStateMachine logToConsole Red $ do
        addTransition Green  ChangeColor Red
        addTransition Red    ChangeColor Green
    
    exitDo Red   $ emitAndWait directionSM ChangeColor
    exitDo Green $ emitAndWait directionSM ChangeColor

    addConditionalTransition Yellow $
        \ChangeColor -> Just <$> takeState directionSM
```