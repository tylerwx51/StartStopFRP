 {-# LANGUAGE RecursiveDo, DoAndIfThenElse, TypeFamilies, FlexibleContexts, ExistentialQuantification, GeneralizedNewtypeDeriving #-}
module Control.StartStop.Lib where

import Control.StartStop.Core
import Control.Monad.Reader
import Control.Monad.Writer.Strict

import Data.IORef

{-
class (MonadFix (Behavior t), Functor (EvStream t), MonadFix (Hold t), Monad (PushOnly t)) => StartStopFRP t where
  data Behavior t :: * -> *
  data EvStream t :: * -> *
  data Hold t :: * -> *
  data PushOnly t :: * -> *

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

instance StartStopFRP (Core.StartStop t) where
  newtype Behavior (Core.StartStop t) a = Behavior { unB :: Core.Behavior t a } deriving(Functor, Applicative, Monad, MonadFix)
  newtype EvStream (Core.StartStop t) a = EvStream { unEv :: Core.EvStream t a } deriving(Functor)
  newtype Hold (Core.StartStop t) a = Hold { unH :: Core.Hold t a } deriving (Functor, Applicative, Monad, MonadFix)
  newtype PushOnly (Core.StartStop t) a = PushOnly { unP :: Core.PushOnly t a } deriving (Functor, Applicative, Monad)

  never = EvStream Core.never
  catMabyeEs = EvStream . Core.catMabyeEs . unEv
  startOnFire = EvStream . Core.startOnFire . fmap unH . unEv
  mergefEs f el er = EvStream $ Core.mergefEs f (unEv el) (unEv er)
  coincidence = EvStream . Core.coincidence . unEv . fmap unEv
  switch = EvStream . Core.switch . unB . fmap unEv
  holdEs e iv = fmap Behavior . Hold $ Core.holdEs (unEv e) iv
  unPushes = EvStream . fmap PushOnly . Core.unPushes . unEv
  pushes = EvStream . Core.pushes . fmap unP . unEv
  sample = Hold . Core.sample . unB
  sampleAfter = Hold . Core.sampleAfter . unB
  changes = EvStream . Core.changes . unB
  unsafePlan = fmap EvStream . Hold . Core.unsafePlan . unEv
-}
{- Starts the given hold, if its returned value is not Nothing. This is
   equivalent to "catMabyeEs . startOnFire".
-}
pull :: EvStream t (Hold t (Maybe a)) -> EvStream t a
pull = catMabyeEs . startOnFire

{-
- slight performance improvement over mergefEs in a few cases. Basicly used for
- an efficient leftmost.
-}
mergefpEs :: (PushOnly t a -> PushOnly t a -> PushOnly t a) -> EvStream t a -> EvStream t a -> EvStream t a
mergefpEs f el er = pushes $ mergefEs f (unPushes el) (unPushes er)

{- merges two EvStreams, if they both fire at the same time, then the left
- value is kept and the right is thrown away.
-}
leftmost :: EvStream t a -> EvStream t a -> EvStream t a
leftmost = mergefpEs const

{-
- For each event that is fired, the function is applied, the new
- EvStream fires if "f a" matches "Just b" and fires with a value of b.
- Is a combo of fmap and filterEs.
-
- e1 = [(1,"a"), (3,"bb"), (7, "ccc")]
- filterMapEs (\str -> if length str > 1 then Just (str ++ "!") else Nothing) == [(3, "bb!"), (7, "ccc!")]
-
- filterMapEs Just = id
- filterMapEs f . filterMapEs g = filterMapEs (f >=> g)
-}
filterMapEs :: (a -> Maybe b) -> EvStream t a -> EvStream t b
filterMapEs f = pull . fmap (return . f)

{-
- Removes any values that when p is applied to the value of the event is False.
-}
filterEs :: (a -> Bool) -> EvStream t a -> EvStream t a
filterEs p = filterMapEs boolToJust
  where
    boolToJust v = if p v then Just v else Nothing


{- If two events fire at the same exact time apply ef to ea-}
data EitherBoth a b = EBLeft a | EBRight b | EBBoth a b
applyEs :: EvStream t (a -> b) -> EvStream t a -> EvStream t b
applyEs ef ea = filterMapEs (\e -> case e of
                                    EBBoth f a -> Just (f a)
                                    _ -> Nothing) $ mergefEs (\(EBLeft f) (EBRight a) -> EBBoth f a) (fmap EBLeft ef) (fmap EBRight ea)

infixl 4 <#>
(<#>) :: EvStream t (a -> b) -> EvStream t a -> EvStream t b
(<#>) = applyEs

gate :: Behavior t Bool -> EvStream t a -> EvStream t a
gate b es = pull $ flip fmap es $ \v -> do
  p <- sample b
  return $ if p then Just v else Nothing

snapshots :: Behavior t a -> EvStream t x -> EvStream t a
snapshots b es = pull $ (Just <$> sample b) <$ es

infixl 4 <@>
(<@>) :: Behavior t (a -> b) -> EvStream t a -> EvStream t b
b <@> es = pull $ flip fmap es $ \a -> do
  f <- sample b
  return $ Just $ f a

foldEs :: (a -> b -> a) -> EvStream t b -> a -> Hold t (Behavior t a)
foldEs f es iv = do
  rec
    let ups = f <$> b <@> es
    b <- holdEs ups iv
  return b

foldEs' :: (a -> b -> a) -> EvStream t b -> a -> Hold t (Behavior t a)
foldEs' f es iv = do
  rec
    ups <- force $ f <$> b <@> es
    b <- holdEs ups iv
  return b

switchEs :: EvStream t a -> EvStream t (EvStream t a) -> Hold t (EvStream t a)
switchEs iv sw = do
  bes <- holdEs sw iv
  return $ switch bes

switcher :: Behavior t a -> EvStream t (Behavior t a) -> Hold t (Behavior t a)
switcher iv es = do
  bb <- holdEs es iv
  return $ join bb

toggle :: EvStream t x -> Bool -> Hold t (Behavior t Bool)
toggle = foldEs' (\v _ -> not v)

count :: (Integral a) => EvStream t x -> Hold t (Behavior t a)
count e = foldEs' (\v _ -> v + 1) e 0

debugIO :: EvStream t (IO ()) -> Hold t ()
debugIO = void . unsafePlan

force :: EvStream t a -> Hold t (EvStream t a)
force = unsafePlan . fmap (\x -> return $! x)
