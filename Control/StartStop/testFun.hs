{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Control.StartStop.TestFun where

import Control.StartStop.Core (Time(..))
import Control.StartStop.Class hiding(E,B,H,P,unE,unB,unH,unP)
import Control.StartStop.Lib (FilterFunctor, filterMap)
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.IORef
import Data.Maybe

runTestHold :: Hold Test (Behavior Test a) -> [a]
runTestHold thb = fmap (b . T) [0 ..]
  where (B b) = unH thb (T 0)

listEvs :: Integer -> EvStream Test a -> [(Time, a)]
listEvs maxTime (E ev) = catMaybes $ do
  t <- fmap T [0..maxTime]
  let mv = ev t
  return $ (\v -> (t, v)) <$> mv

listToEv :: [(Time, a)] -> EvStream Test a
listToEv vs = E $ \t -> lookup t vs

lastFired :: Time -> EvStream Test a -> Maybe (Time, a)
lastFired (T (-1)) _ = Nothing
lastFired t@(T i) (E ev) = case ev t of
                  Nothing -> lastFired (T (i - 1)) (E ev)
                  Just v -> Just (t, v)


instance Functor (EvStream Test) where
  fmap f es = E $ \t -> f <$> unE es t

instance FilterFunctor (EvStream Test) where
  filterMap f es = E $ unE es >=> f

data Test
instance StartStopFRP Test where
  newtype EvStream Test a = E { unE :: Time -> Maybe a }
  newtype Behavior Test a = B { unB :: Time -> a } deriving (Functor, Applicative, Monad, MonadFix)
  newtype Hold Test a = H { unH :: Time -> a } deriving (Functor, Applicative, Monad, MonadFix)
  newtype PushOnly Test a = P { unP :: Identity a } deriving (Functor, Applicative, Monad, MonadFix)

  never = E $ const Nothing
  switch b = E $ \t -> unE (unB b t) t
  coincidence (E ev) = E $ \t -> ev t >>= \(E e2) -> e2 t
  unPushes = fmap (P . Identity)
  pushes = fmap (runIdentity . unP)
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
  unsafeIOMap = undefined
