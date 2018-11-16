{-# Language TemplateHaskell #-}
import           Universum
import           Test.Hspec.Extra

import           Data.Flag
import           Control.Loger
import           Control.Actor
import           Control.StateMachine

actorPingPongTest :: IO Bool
actorPingPongTest = finishFor 1000 $ do
    success <- newFlag
    actor1  <- makeActor (handlers success)
    actor2  <- makeActor (handlers success)
    notify actor2 $ Ping actor1 10
    await success

data Ping = Ping Actor Int
data Pong = Pong Actor Int

ping :: Flag -> Actor -> Ping -> IO ()
ping sem  _      (Ping _     0) = liftFlag sem
ping _    link (Ping actor n) = notify actor $ Pong link (n-1)

pong :: Flag -> Actor -> Pong -> IO ()
pong sem  _      (Pong _     0) = liftFlag sem
pong _    link (Pong actor n) = notify actor $ Ping link (n-1)

handlers :: Flag -> Actor -> HandlerL ()
handlers qSem link = do
    math $ ping qSem link
    math $ pong qSem link


makeStates ["On", "Off"]
makeEvents ["TakeOn"]

stateMachinTest1 :: IO Bool
stateMachinTest1 = finishFor 100 $ do
    success <- newFlag
    sm  <- runStateMachine logOff Off $ do
        addTransition  Off TakeOn On
        setFinishState On
        entryDo        On  $ liftFlag success
        exitDo         Off $ pure ()
        exitDo         On  $ pure ()
    emit sm TakeOn
    await success

stateMachinTest2 :: IO Bool
stateMachinTest2 = finishFor 1000 $ do
    success <- newFlag
    sm      <- runStateMachine logOff Off $ do
        addConditionalTransition Off $
            \pressing -> if pressing == StrongPress then just On else nothing 
        addConditionalTransition On $
            \pressing -> if pressing == StrongPress then just Off else nothing

        transitionDo On Off $ \(_ :: Press) -> liftFlag success

    emit sm StrongPress
    emit sm StrongPress
    await success

data Press = StrongPress | WeaklyPress deriving Eq

main :: IO ()
main = do
    putTextLn ""
    hspec $ do
        it "Actor ping pong test"     $ isOk actorPingPongTest
        it "Test 1 for state machine" $ isOk stateMachinTest1
        it "Test 2 for state machine" $ isOk stateMachinTest2




{-
makeStates ["S1", "S2", "S3"]
makeEvents ["Cliked"]

stateMachinTest3 :: IO StateMachine
stateMachinTest3 = runStateMachine logOff S1 $ do
    addTransition  S1 Cliked S2
    addTransition  S2 Cliked S3
    addTransition  S3 Cliked S1
-}
    {-

QStateMachine machine;
QState *s1 = new QState();
QState *s2 = new QState();
QState *s3 = new QState();

s1->addTransition(button, SIGNAL(clicked()), s2);
s2->addTransition(button, SIGNAL(clicked()), s3);
s3->addTransition(button, SIGNAL(clicked()), s1);

machine.addState(s1);
machine.addState(s2);
machine.addState(s3);
machine.setInitialState(s1);

machine.start();
-}