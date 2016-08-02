{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Control.StartStop.Class where

import Control.Monad.Fix
import qualified Control.StartStop.Core as Core

class (MonadFix (Behavior t), Functor (EvStream t), MonadFix (Hold t), MonadFix (PushOnly t)) => StartStopFRP t where
  type Behavior t :: * -> *
  type EvStream t :: * -> *
  type Hold t :: * -> *
  type PushOnly t :: * -> *

  never :: EvStream t a
  catMabyeEs :: EvStream t (Maybe a) -> EvStream t a
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
  type Behavior (Core t) = Core.Behavior t
  type EvStream (Core t) = Core.EvStream t
  type Hold (Core t) = Core.Hold t
  type PushOnly (Core t) = Core.PushOnly t

  never = Core.never
  catMabyeEs = Core.catMabyeEs
  startOnFire = Core.startOnFire
  mergefEs = Core.mergefEs
  coincidence = Core.coincidence
  switch = Core.switch
  holdEs = Core.holdEs
  unPushes = Core.unPushes
  pushes = Core.pushes
  sample = Core.sample
  sampleAfter = Core.sampleAfter
  changes = Core.changes
  unsafePlan = Core.unsafePlan
  unsafeIOMap = Core.unsafeIOMap
