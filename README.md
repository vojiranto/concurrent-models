
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
    actor1  <- runActor logOff $ \link -> do
        math $ ping success link
        math $ pong success link

    actor2  <- runActor logOff $ \link -> do
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

# SM

Module Control.StateMachine is framework to build asynchronous FSM. What is supported?

Let's demonstrate with examples.

The simplest example of a finite state machine. Three states, two transitions, one state initial, one final.

```haskell
data S1 = S1
data S2 = S2
data S3 = S3
data Event = Event

sequentialStateMachine :: IO ()
sequentialStateMachine = do
    stopSM <- newFlag
    sm     <- runStateMachine logToConsole S1 $ do
        addTransition  S1 Event S2
        addTransition  S2 Event S3
        setFinishState S3
        exitDo S3 $ liftFlag stopSM
    sm `emit` Event
    sm `emit` Event
    wait stopSM
```

The execution log for this state machine

```
[SM] [1118SuUnP1] [set transition] [state S1] -> [event Event] -> [state S2]
[SM] [1118SuUnP1] [set transition] [state S2] -> [event Event] -> [state S3]
[SM] [1118SuUnP1] [set finish state] [state S3]
[SM] [1118SuUnP1] [set 'exit do' handler] [state S3]
[SM] [1118SuUnP1] [init state] [state S1]
[SM] [1118SuUnP1] [transition] [state S1] -> [event Event] -> [state S2]
[SM] [1118SuUnP1] [transition] [state S2] -> [event Event] -> [state S3]
[SM] [1118SuUnP1] [finish state] [state S3]
[SM] [1118SuUnP1] [exit do] [state S3]
```

On the example of a traffic light, we show the dynamic choice of the transition.
Request from the machine of its current state.
As well as the use of IO actions during the construction of the machine.

```haskell
makeStates ["Green", "Yellow", "Red"]
makeEvents ["ChangeColor"]

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
```

TODO: Code examle for items:
4. State grouping, you can also group groups into higher order groups.
5. Addition transition from goup.
6. Handlers state and groups entry/exit.
7. Handlers for events.