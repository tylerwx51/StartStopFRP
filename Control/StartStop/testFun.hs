{-# LANGUAGE RecursiveDo #-}
module Control.StartStop.TestFun where

import Control.StartStop.Core
import Control.StartStop.EvPrim
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.IORef
import Data.Maybe

type TestEvStream a = Time -> Maybe a
type TestBehavior a = Time -> a
type TestHold a = Time -> a

runTestHold :: TestHold (TestBehavior a) -> [a]
runTestHold thb = fmap (b . T) [0 ..]
  where b = thb (T 0)

listEvs :: Integer -> TestEvStream a -> [(Time, a)]
listEvs maxTime ev = catMaybes $ do
  t <- fmap T [0..maxTime]
  let mv = ev t
  return $ (\v -> (t, v)) <$> mv

listToEv :: [(Time, a)] -> TestEvStream a
listToEv vs t = lookup t vs

lastFired :: Time -> TestEvStream a -> Maybe (Time, a)
lastFired (T (-1)) _ = Nothing
lastFired t@(T i) ev = case ev t of
                  Nothing -> lastFired (T (i - 1)) ev
                  Just v -> Just (t, v)

neverTest :: TestEvStream a
neverTest _ = Nothing

testSwitch :: TestBehavior (TestEvStream a) -> TestEvStream a
testSwitch b = \t -> b t t

holdTest :: TestEvStream a -> a -> TestHold (TestBehavior a)
holdTest evs iv startTime = \(T i) -> case lastFired (T (i - 1)) evs of
                                    Just (t',v)
                                      | startTime < t' -> v
                                    _ -> iv

testMergef :: (a -> a -> a) -> TestEvStream a -> TestEvStream a -> TestEvStream a
testMergef f el er t = case (el t, er t) of
  (Just l, Nothing) -> Just l
  (Nothing, Just r) -> Just r
  (Just l, Just r) -> Just $ f l r
  (Nothing, Nothing) -> Nothing

startOnFireTest :: TestEvStream (TestHold a) -> TestEvStream a
startOnFireTest ev t = fmap ($ t) (ev t)

testSample :: TestBehavior a -> TestHold a
testSample = id
