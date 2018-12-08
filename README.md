
For a complete list of features use `stack haddock` :) 

# Actor

What can actors do? They can: receive messages, send messages, create new actors and change their internal state.

```haskell
module Actor.PingPong where

import           Universum

import           Control.Concurrent.Model
import           Control.Concurrent.Flag        -- To report about successful completion.
import           Control.Concurrent.Actor
import           Control.Concurrent.Loger

-- You can send different types of messages to actors.
-- If there is no suitable handler, then it will simply be droped.
data Ping = Ping Actor Int
data Pong = Pong Actor Int

actorPingPong :: IO ()
actorPingPong = do
    success <- newFlag
    pingPong success
    wait success

pingPong :: Flag -> IO ()
pingPong success = do 
    actor1 <- pinger success
    actor2 <- pinger success
    notify actor2 $ Ping actor1 10

pinger :: Flag -> IO Actor
pinger success = runActor loger $ do
    link <- this
    math $ ping success link
    math $ pong success link

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
data SequentialState1 = SequentialState1
data SequentialState2 = SequentialState2
data SequentialState3 = SequentialState3

data EkzampleEvent = EkzampleEvent

sequentialStateMachine :: IO ()
sequentialStateMachine = do
    stopSM <- newFlag
    sm    <- runStateMachine loger SequentialState1 $ do
        ifE EkzampleEvent $ SequentialState1 >-> SequentialState2
        ifE EkzampleEvent $ SequentialState2 >-> SequentialState3
        addFinalState SequentialState3
        onExit SequentialState3 $ liftFlag stopSM
    sm `notify` EkzampleEvent
    sm `notify` EkzampleEvent
    wait stopSM
```

On the example of a traffic light, we show the dynamic choice of the transition.
Request from the machine of its current state.
As well as the use of IO actions during the construction of the machine.

```haskell
makeStates ["Green", "Yellow", "Red"]
makeEvents ["ChangeColor"]

-- properly functioning traffic lights
makeTrafficLight2 :: IO StateMachine
makeTrafficLight2 = runStateMachine loger Green $ do
    -- add to fsm transitions from one states to another.
    ifE ChangeColor $ Green >-> Yellow
    ifE ChangeColor $ Red   >-> Yellow

    -- during the construction of the FSN, you can use IO.
    directionSM <- liftIO $ runStateMachine loger Red $ do
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
    sm <- runStateMachine loger S1 $ do
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