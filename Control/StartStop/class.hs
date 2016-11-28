{-# LANGUAGE TypeFamilies, FlexibleContexts, RecursiveDo #-}
module Control.StartStop.Class where

import Data.Monoid

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g x y = f (g x y)
infixr 9 .:

class (Functor f) => FilterFunctor f where
  filterMap :: (a -> Maybe b) -> f a -> f b
  filterMap f = catMaybes . fmap f

  catMaybes :: f (Maybe a) -> f a
  catMaybes = filterMap id

    {--
    - laws :
    - filterMap Just = id
    - filterMap f . filterMap g = filterMap (f <=< g)
    --}

instance FilterFunctor [] where
  filterMap _ [] = []
  filterMap f (x : xs) = case f x of
    Nothing -> filterMap f xs
    Just y -> y : filterMap f xs

instance FilterFunctor Maybe where
  filterMap = (=<<)

ffilter :: (FilterFunctor f) => (a -> Bool) -> f a -> f a
ffilter p = filterMap (\v -> boolToMaybe (p v) v)
  where
    boolToMaybe b v = if b then Just v else Nothing

data FireTime a b = LeftFired a
                  | RightFired b
                  | Simul a b
                  deriving (Show)

class (FilterFunctor (EvStream t), Monad (Reactive t), MonadFix (Behavior t)) => StartStop t where
  data Behavior t a
  data Reactive t a
  data EvStream t a

  never :: EvStream t a
  merge :: EvStream t a -> EvStream t b -> EvStream t (FireTime a b)
  samples :: EvStream t (Behavior t a) -> EvStream t a
  switch :: Behavior t (EvStream t a) -> EvStream t a
  coincidence :: EvStream t (EvStream t a) -> EvStream t a
  holdEs :: a -> EvStream t a -> Behavior t (Reactive t a)
  sampleR :: Reactive t a -> Behavior t a
  sampleRAfter :: Reactive t a -> Behavior t a
  changes :: Reactive t a -> EvStream t a

class (StartStop t, MonadIO (PlanIO t), MonadFix (PlanIO t), MonadFix (PlanHold t), MonadIO (PlanHold t)) => StartStopIO t where
  data PlanHold t a
  data PlanIO t a
  callbackStream :: PlanHold t (a -> IO (), EvStream t [a])
  planEs :: EvStream t (PlanIO t a) -> PlanHold t (EvStream t a)
  sampleB :: Behavior t a -> PlanHold t a

mergeWith :: (StartStop t) => (a -> a -> a) -> EvStream t a -> EvStream t a -> EvStream t a
mergeWith f = fmap cmp .: merge
  where
    cmp (LeftFired x) = x
    cmp (RightFired y) = y
    cmp (Simul x y) = f x y

leftmost :: (Foldable f, StartStop t) => f (EvStream t a) -> EvStream t a
leftmost = mergefEs const

mergefEs :: (Foldable f, StartStop t) => (a -> a -> a) -> f (EvStream t a) -> EvStream t a
mergefEs f = foldr (mergeWith f) never

snapshots :: (StartStop t) => Behavior t a -> EvStream t x -> EvStream t a
snapshots b = samples . fmap (const b)

snapshotsR :: (StartStop t) => Reactive t a -> EvStream t x -> EvStream t a
snapshotsR = snapshots . sampleR

gate :: (StartStop t) => Behavior t Bool -> EvStream t a -> EvStream t a
gate b es = catMaybes . samples $ flip fmap es $ \v -> do
  p <- b
  return $ if p then Just v else Nothing

infixl 4 <@>
(<@>) :: (StartStop t) => Behavior t (a -> b) -> EvStream t a -> EvStream t b
bf <@> es = samples $ flip fmap es $ \a -> bf >>= \f -> return (f a)

foldEs :: (StartStop t) => (a -> b -> a) -> a -> EvStream t b -> Behavior t (Reactive t a)
foldEs f iv es = do
  rec
    let ups = f <$> sampleR r <@> es
    r <- holdEs iv ups
  return r

switcher :: (StartStop t) => Reactive t a -> EvStream t (Reactive t a) -> Behavior t (Reactive t a)
switcher = fmap join .: holdEs

data Updates t s = Updates (s -> (Updates t s, EvStream t (s -> s)))
                 | NoUpdate

updater :: (StartStop t) => (s -> EvStream t (s -> s)) -> Updates t s
updater f = Updates ((\e -> (NoUpdate, e)) . f)

instance (StartStop t) => Monoid (Updates t s) where
  mempty = NoUpdate
  mappend NoUpdate u = u
  mappend u NoUpdate = u
  mappend (Updates f) u2 = Updates ((\(u, e) -> (u <> u2, e)) . f)

runUpdates :: (StartStop t) => s -> Updates t s -> Behavior t (Reactive t s)
runUpdates iv updates = do
  rec
    let ups = switch $ sampleR $ fmap (flip updateEvStream updates) rState
    rState <- foldEs (\s f -> f s) iv ups

  return rState

updateEvStream :: (StartStop t) => s -> Updates t s -> EvStream t (s -> s)
updateEvStream _ NoUpdate = never
updateEvStream state (Updates f) = nextFire
  where
    (next, evs) = f state
    nextEvs = updateEvStream state next
    nextFire = leftmost [coincidence $ fmap (\fs -> updateEvStream (fs state) next) evs, evs, nextEvs]
