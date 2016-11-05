{-# LANGUAGE GADTs, TypeOperators #-}

module Control.StartStop.ReadPhase where

import Control.StartStop.Free
import Data.IORef
import Data.Unique
import Data.Functor.Compose
import System.Mem.Weak

data Time

type Phase t = FreeFix (ReadPhase t) `Compose` FreeFix (WritePhase t) `Compose` FreeFix (CleanUpPhase t)
data WritePhase t a
data CleanUpPhase t a

type Ref a = IORef a
data ReadPhase t a where
  NewRef :: a -> ReadPhase t (Ref a)
  ReadRef :: Ref a -> ReadPhase t a
  MkWeakRef :: Ref a -> ReadPhase t (Weak (Ref a))
  GetTime :: ReadPhase t Time
  NewUnique :: ReadPhase t Unique

readRef :: Ref a -> FreeFix (ReadPhase t) a
readRef = liftFF . ReadRef

newRef :: a -> FreeFix (ReadPhase t) (Ref a)
newRef = liftFF . NewRef

getCurTime :: FreeFix (ReadPhase t) Time
getCurTime = liftFF GetTime

newUnique :: FreeFix (ReadPhase t) Unique
newUnique = liftFF NewUnique

mkWeakRef :: Ref a -> FreeFix (ReadPhase t) (Weak (Ref a))
mkWeakRef = liftFF . MkWeakRef

read2Phase :: ReadPhase t ((FreeFix (WritePhase t) `Compose` FreeFix (CleanUpPhase t)) a) -> Phase t a
read2Phase = Compose . liftFF
