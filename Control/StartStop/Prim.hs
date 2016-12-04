{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Control.StartStop.Prim where

import Control.Monad
import Control.Monad.Reader

import Data.Map.Strict as Map
import Data.Functor.Compose
import Data.IORef
import Data.Unique

import System.IO.Unsafe

{-
- Time is used internally in StartStopFRP. In this implementation it is an integer
- with the round number.
-}
newtype Time = T Integer deriving (Eq, Ord, Show)

newtype Sample t a = Sample { unSample :: ReaderT Time IO a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

newtype WritePhase t a = WritePhase { runWritePhase :: IO a } deriving (Functor, Applicative, Monad)

writeIORefWrite :: IORef a -> a -> Phase t ()
writeIORefWrite ref v = Compose $ return $ Compose $ (WritePhase $ writeIORef ref v) >> return (pure ())

newtype CleanUpPhase t a = CleanUpPhase { runCleanUpPhase :: IO a } deriving (Functor, Applicative, Monad)

writeIORefClean :: IORef a -> a -> CleanUpPhase t ()
writeIORefClean ref v = CleanUpPhase $ writeIORef ref v

runAndCleanUp :: (RoundSequence t -> CleanUpPhase t ()) -> RoundSequence t -> Phase t ()
runAndCleanUp f rs = Compose $ do
  writePhase <- getCompose $ runRoundSequence rs
  return $ Compose $ do
    cleanUpPhase <- getCompose writePhase
    return $ do
      rsNew <- cleanUpPhase
      f rsNew

type Phase t a = (Sample t `Compose` WritePhase t `Compose` CleanUpPhase t) a

{-
- RoundAction contains a series of instructions for what to update after each frame
- of the FRP. It will be in charge of determining what values to write to any refrences
- then writing those values to those refrences.
-
- NoAction represents an RoundAction that never updates anything. This is used to
- so that collections of RoundActions can remove any unnecissary RoundActions.
-
- RoundAction phase is used for any actions that update things. It just takes a Phase
- that gives the instructions for how to update the correct IORefs. The value returned
- by the Phase is an updated version of the RoundAction. If the RoundAction is no longer
- necissary it can return NoAction. It could also become a simpiler RoundAction as well.
- Using the original RoundAction or the returned RoundAction should always result in
- the same observable effects. However the returned RoundAction is more efficient or the same
- as the original RoundAction.
-}
data RoundAction t = NoAction
                   | RoundAction (Phase t (RoundAction t))

{- RoundSequence is a collection of RoundActions. Each RoundAction is given a unique key.
- This is done so that repetativly appending the same RoundAction will not build up
- useless actions. The Monoid for RoundSequence concatonates two RoundSequences.
-}
newtype RoundSequence t = RoundSequence (Map Unique (RoundAction t)) deriving (Monoid)

{- "isNoAction" is a helper function that finds out if an action is NoAction -}
isNoAction :: RoundAction t -> Bool
isNoAction NoAction = True
isNoAction _ = False

{- "runRoundAction" gives a Phase with the instructions to perform the Round Action -}
runRoundAction :: RoundAction t -> Phase t (RoundAction t)
runRoundAction NoAction = pure NoAction
runRoundAction (RoundAction p) = p

{- "runRoundAction" gives a phase with the instructions to run all of the RoundActions
- in the RoundSequence.
-}
runRoundSequence :: RoundSequence t -> Phase t (RoundSequence t)
runRoundSequence (RoundSequence phaseMap) = RoundSequence . Map.filter (not . isNoAction) <$> traverse runRoundAction phaseMap

{- Creates a new RoundSequence with just one RoundAction. This requires a unique identifier
- to be grabbed, so it must be done in a Sample. The resulting RoundSequence can be concated
- with other RoundSequences.
-}
newRoundSequence :: RoundAction t -> Sample t (RoundSequence t)
newRoundSequence action = do
  identifier <- liftIO newUnique
  return $ RoundSequence $ singleton identifier action

{- Converts a series of phase instructions into a ReaderT Time IO a, that
- performs all of the instructions of the Phase.
-}
runPhase :: Phase t a -> ReaderT Time IO a
runPhase phase = do
  writePhase <- unSample $ getCompose phase
  cleanUpPhase <- lift $ runWritePhase $ getCompose writePhase
  lift $ runCleanUpPhase cleanUpPhase

getCurTime :: Sample t Time
getCurTime = Sample ask

{-# NOINLINE memoSample #-}
memoSample :: Sample t a -> Sample t a
memoSample s = unsafePerformIO $ do
  currentValRef <- newIORef Nothing
  return $ usePrevSample currentValRef s

{-
- Since samples are guaranteed to return the same value given a time t
- we can store the value of the sample at a given time. The IORef will
- store the most recent value when the sample was run, and when it was run.
- If the IORef holds a value that was run during the same round, then it
- uses that value instead of recalcuating.
--}
usePrevSample :: IORef (Maybe (Time, a)) -> Sample t a -> Sample t a
usePrevSample currentValRef recalcSample = do
  mtv <- liftIO $ readIORef currentValRef
  curTime <- getCurTime
  case mtv of
    Just (t, v)
      | t == curTime -> return v
    _ -> do
      a <- recalcSample
      liftIO $ writeIORef currentValRef (Just (curTime, a))
      return a
