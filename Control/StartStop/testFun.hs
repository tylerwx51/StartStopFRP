{-# LANGUAGE RecursiveDo, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Control.StartStop.TestFun where

import Control.StartStop.Core (Time(..))
import Control.StartStop.Class
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.IORef
import Data.Maybe

runTestHold :: H (B a) -> [a]
runTestHold thb = fmap (b . T) [0 ..]
  where (B b) = unH thb (T 0)

listEvs :: Integer -> E a -> [(Time, a)]
listEvs maxTime (E ev) = catMaybes $ do
  t <- fmap T [0..maxTime]
  let mv = ev t
  return $ (\v -> (t, v)) <$> mv

listToEv :: [(Time, a)] -> E a
listToEv vs = E $ \t -> lookup t vs

lastFired :: Time -> E a -> Maybe (Time, a)
lastFired (T (-1)) _ = Nothing
lastFired t@(T i) (E ev) = case ev t of
                  Nothing -> lastFired (T (i - 1)) (E ev)
                  Just v -> Just (t, v)

data Test

newtype E a = E { unE :: Time -> Maybe a }
newtype B a = B { unB :: Time -> a } deriving (Functor, Applicative, Monad, MonadFix)
newtype H a = H { unH :: Time -> a } deriving (Functor, Applicative, Monad, MonadFix)

instance Functor E where
  fmap f (E e) = E $ e >=> Just . f

instance StartStopFRP Test where
  type EvStream Test = E
  type Behavior Test = B
  type Hold Test = H
  type PushOnly Test = Identity

  never = E $ const Nothing
  switch b = E $ \t -> unE (unB b t) t
  catMabyeEs (E ev) = E $ join . ev
  coincidence (E ev) = E $ \t -> ev t >>= \(E e2) -> e2 t
  unPushes = fmap Identity
  pushes = fmap runIdentity
  holdEs evs iv = H $ \startTime -> B $ \(T i) -> case lastFired (T (i - 1)) evs of
                                      Just (t',v)
                                        | startTime < t' -> v
                                      _ -> iv

  mergefEs f el er = E $ \t -> case (unE el t, unE er t) of
    (Just l, Nothing) -> Just l
    (Nothing, Just r) -> Just r
    (Just l, Just r) -> Just $ f l r
    (Nothing, Nothing) -> Nothing

  startOnFire (E ev) = E $ \t -> fmap ($ t) (unH <$> ev t)
  sample b = H $ \t -> unB b t
  sampleAfter (B b) = H $ \(T i) -> b (T (i + 1))

  changes = undefined
  unsafePlan = undefined
