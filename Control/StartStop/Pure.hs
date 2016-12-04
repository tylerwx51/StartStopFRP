{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveFunctor, FlexibleInstances, Rank2Types #-}

module Control.StartStop.Pure where

import Control.StartStop.Class
import Control.Monad.Fix
import Control.Monad

import Data.Maybe

data Pure
type Time = Integer
instance StartStop Pure where
  newtype Behavior Pure a = B { runB :: Time -> a } deriving (Functor, Applicative, Monad, MonadFix)
  newtype EvStream Pure a = E { runE :: Time -> Maybe a } deriving (Functor)
  newtype Reactive Pure a = R { runR :: Time -> (a, Maybe a) }

  never = E (return Nothing)
  merge e1 e2 = E $ do
    mv1 <- runE e1
    mv2 <- runE e2
    case (mv1, mv2) of
      (Nothing, Nothing) -> return Nothing
      (Just v1, Nothing) -> return $ Just $ LeftFired v1
      (Nothing, Just v2) -> return $ Just $ RightFired v2
      (Just v1, Just v2) -> return $ Just $ Simul v1 v2

  samples eb = E $ do
    mb <- runE eb
    case mb of
      Nothing -> return Nothing
      Just b -> fmap Just (runB b)

  switch be = E $ do
    e <- runB be
    runE e

  coincidence ee = E $ do
    me <- runE ee
    case me of
      Nothing -> return Nothing
      Just e -> runE e

  holdEs iv evs = B $ do
    startTime <- id
    let lookupLast t
          | t <= startTime = iv
          | otherwise = fromMaybe (lookupLast (t - 1)) (runE evs t)

    return $ R $ do
      t <- id
      return (lookupLast (t - 1), runE evs t)

  sampleR = B . fmap fst . runR
  sampleRAfter r = B $ do
    (v, mf) <- runR r
    return $ fromMaybe v mf
  changes = E . fmap snd . runR

instance FilterFunctor (EvStream Pure) where
  filterMap f = E . fmap (>>= f) . runE

instance Functor (Reactive Pure) where
  fmap = liftM

instance Applicative (Reactive Pure) where
  pure = return
  (<*>) = ap

instance Monad (Reactive Pure) where
  return x = R $ return (x, Nothing)
  r >>= f = R $ do
    (currentA, futureA) <- runR r
    (currentB, _) <- runR $ f currentA
    (_, futureB) <- runR $ f (fromMaybe currentA futureA)
    return (currentB, futureB)

runPure :: [(Time, a)] -> (forall t . (StartStop t) => EvStream t a -> Behavior t (Reactive t b)) -> (Time -> b)
runPure es f = fst <$> runR (runB (f $ E $ flip lookup es) 0)
