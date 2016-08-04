{-# LANGUAGE TypeFamilies, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Control.StartStop.Class where

import Control.StartStop.Lib
import Control.Monad.Fix
import qualified Control.StartStop.Core as Core

class (MonadFix (Behavior t), FilterFunctor (EvStream t), MonadFix (Hold t), MonadFix (PushOnly t)) => StartStopFRP t where
  data Behavior t :: * -> *
  data EvStream t :: * -> *
  data Hold t :: * -> *
  data PushOnly t :: * -> *

  never :: EvStream t a
  startOnFire :: EvStream t (Hold t a) -> EvStream t a
  mergefEs :: (a -> a -> a) -> EvStream t a -> EvStream t a -> EvStream t a
  coincidence :: EvStream t (EvStream t a) -> EvStream t a
  switch :: Behavior t (EvStream t a) -> EvStream t a
  holdEs :: EvStream t a -> a -> Hold t (Behavior t a)
  unPushes :: EvStream t a -> EvStream t (PushOnly t a)
  pushes :: EvStream t (PushOnly t a) -> EvStream t a
  sample :: Behavior t a -> Hold t a
  sampleAfter :: Behavior t a -> Hold t a
  changes :: Behavior t a -> EvStream t a
  unsafePlan :: EvStream t (IO a) -> Hold t (EvStream t a)
  unsafeIOMap :: EvStream t (IO a) -> EvStream t a

data Core t
instance StartStopFRP (Core t) where
  newtype Behavior (Core t) a = B { unB :: Core.Behavior t a } deriving(Functor,Applicative,Monad,MonadFix)
  newtype EvStream (Core t) a = E { unE :: Core.EvStream t a } deriving(Functor, FilterFunctor)
  newtype Hold (Core t) a = H { unH :: Core.Hold t a } deriving(Functor,Applicative,Monad,MonadFix)
  newtype PushOnly (Core t) a = P { unP :: Core.PushOnly t a } deriving(Functor,Applicative,Monad,MonadFix)

  never = E Core.never
  startOnFire = E . Core.startOnFire . fmap unH . unE
  mergefEs f (E e1) (E e2) = E $ Core.mergefEs f e1 e2
  coincidence (E ee) = E $ Core.coincidence $ fmap unE ee
  switch (B b) = E $ Core.switch $ fmap unE b
  holdEs (E e) iv = H . fmap B $ Core.holdEs e iv
  unPushes = E . fmap P . Core.unPushes . unE
  pushes = E . Core.pushes . fmap unP . unE
  sample = H . Core.sample . unB
  sampleAfter = H . Core.sampleAfter . unB
  changes = E . Core.changes . unB
  unsafePlan = H . fmap E . Core.unsafePlan . unE
  unsafeIOMap = E . Core.unsafeIOMap . unE
