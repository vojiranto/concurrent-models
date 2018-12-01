For a complete list of features use `stack haddock` :) 

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
    actor1  <- runActor logOff $ do
        link <- this
        math $ ping success link
        math $ pong success link

    actor2  <- runActor logOff $ do
        link <- this
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

# State machine

Module Control.StateMachine is a framework to build asynchronous FSMs. What is supported?

Let's demonstrate with examples.

The simplest example of a finite state machine. Three states, two transitions, one state initial, one final.

```haskell
data S1 = S1
data S2 = S2
data S3 = S3
data EkzampleEvent = EkzampleEvent

sequentialStateMachine :: IO ()
sequentialStateMachine = do
    stopSM <- newFlag
    sm     <- runStateMachine logOff S1 $ do
        ifE EkzampleEvent $ S1 >-> S2
        ifE EkzampleEvent $ S2 >-> S3
        addFinalState S3
        onExit S3 $ liftFlag stopSM
    sm `notify` EkzampleEvent
    sm `notify` EkzampleEvent
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

-- properly functioning traffic lights
makeTrafficLight2 :: IO StateMachine
makeTrafficLight2 = runStateMachine logToConsole Green $ do
    -- add to fsm transitions from one states to another.
    ifE ChangeColor $ Green >-> Yellow
    ifE ChangeColor $ Red   >-> Yellow

    -- during the construction of the FSN, you can use IO.
    directionSM <- liftIO $ runStateMachine logToConsole Red $ do
        ifE ChangeColor $ Green >-> Red
        ifE ChangeColor $ Red   >-> Green

    onExit Red   $ notifyAndWait directionSM ChangeColor
    onExit Green $ notifyAndWait directionSM ChangeColor

    -- add context dependent transition
    -- naturally, you can also use logical conditions for events.
    Yellow >?> \ChangeColor -> Just <$> readState directionSM
```

You can collect states in groups and add to group common handlers and transitions to other states.

```haskell
data S1 = S1
data S2 = S2
data S3 = S3
data G  = G
data FS = FS

data Move = Move
data Exit = Exit

groupingStateMachine :: IO ()
groupingStateMachine = do
    stopSM <- newFlag
    sm <- runStateMachine logOff S1 $ do
        ifE Move $ S1 >-> S2
        ifE Move $ S2 >-> S3
        ifE Move $ S3 >-> S1

        groupStates    G $ S1 <: S2 <: S3 <: []
        ifE Exit     $ G >-> FS

        addFinalState FS
        onExit FS $ liftFlag stopSM

    sm `notify` Move
    sm `notify` Exit

    wait stopSM
```

```
[SM] [1118St6C9u] [set transition] [state S1] -> [event Move] -> [state S2]
[SM] [1118St6C9u] [set transition] [state S2] -> [event Move] -> [state S3]
[SM] [1118St6C9u] [set transition] [state S3] -> [event Move] -> [state S1]
[SM] [1118St6C9u] [make group] [state G]
[SM] [1118St6C9u] [add states to group] [state S1]
[SM] [1118St6C9u] [add states to group] [state S2]
[SM] [1118St6C9u] [add states to group] [state S3]
[SM] [1118St6C9u] [set transition] [state G] -> [event Exit] -> [state FS]
[SM] [1118St6C9u] [set 'exit do' handler] [state G]
[SM] [1118St6C9u] [set finish state] [state FS]
[SM] [1118St6C9u] [set 'exit do' handler] [state FS]
[SM] [1118St6C9u] [init state] [state S1]
[SM] [1118St6C9u] [transition] [state S1] -> [event Move] -> [state S2]
[SM] [1118St6C9u] [transition] [state S2] -> [event Exit] -> [state FS]
[SM] [1118St6C9u] [exit do] [state G]
[SM] [1118St6C9u] [finish state] [state FS]
[SM] [1118St6C9u] [exit do] [state FS]
```