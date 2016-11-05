 {-# LANGUAGE RecursiveDo, DoAndIfThenElse, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}
module Control.StartStop.Lib where

import Control.StartStop.Core
import Control.Monad
import Data.Monoid


{- Starts the given hold, if its returned value is not Nothing. This is
   equivalent to "catMabyeEs . startOnFire".
-}
pull :: EvStream t (Behavior t (Maybe a)) -> EvStream t a
pull = catMaybes . startOnFire

{-
- slight performance improvement over mergefEs in a few cases. Basicly used for
- an efficient leftmost.

mergefpEs :: (PushOnly t a -> PushOnly t a -> PushOnly t a) -> EvStream t a -> EvStream t a -> EvStream t a
mergefpEs f el er = pushes $ mergefEs f (unPushes el) (unPushes er)
-}
{- merges two EvStreams, if they both fire at the same time, then the left
- value is kept and the right is thrown away.
-}
leftmost :: EvStream t a -> EvStream t a -> EvStream t a
leftmost = mergefEs const

{- If two events fire at the same exact time apply ef to ea-}
data EitherBoth a b = EBLeft a | EBRight b | EBBoth a b
applyEs :: EvStream t (a -> b) -> EvStream t a -> EvStream t b
applyEs ef ea = filterMap (\e -> case e of
                                    EBBoth f a -> Just (f a)
                                    _ -> Nothing) $ mergefEs (\(EBLeft f) (EBRight a) -> EBBoth f a) (fmap EBLeft ef) (fmap EBRight ea)

infixl 4 <#>
(<#>) :: EvStream t (a -> b) -> EvStream t a -> EvStream t b
(<#>) = applyEs

{- "gate b e" fires whenever e fires and when the value of b is True. -}
gate :: Reactive t Bool -> EvStream t a -> EvStream t a
gate b es = pull $ flip fmap es $ \v -> do
  p <- liftReactive b
  return $ if p then Just v else Nothing

{- "snapshots b e" fires whenever e fires with the value of b when e fires.-}
snapshots :: Reactive t a -> EvStream t x -> EvStream t a
snapshots b es = pull $ (Just <$> liftReactive b) <$ es

infixl 4 <@>
(<@>) :: Reactive t (a -> b) -> EvStream t a -> EvStream t b
b <@> es = pull $ flip fmap es $ \a -> do
  f <- liftReactive b
  return $ Just $ f a

foldEs :: (a -> b -> a) -> a -> EvStream t b -> Behavior t (Reactive t a)
foldEs f iv es = do
  rec
    let ups = f <$> b <@> es
    b <- holdEs iv ups
  return b

switchEs :: EvStream t a -> EvStream t (EvStream t a) -> Behavior t (EvStream t a)
switchEs iv sw = do
  bes <- holdEs iv sw
  return $ switch bes

switcher :: Reactive t a -> EvStream t (Reactive t a) -> Behavior t (Reactive t a)
switcher iv es = do
  bb <- holdEs iv es
  return $ join bb

toggle :: Bool -> EvStream t x -> Behavior t (Reactive t Bool)
toggle = foldEs (\v _ -> not v)

count :: (Integral a) => EvStream t x -> Behavior t (Reactive t a)
count = foldEs (\v _ -> v + 1) 0

debugIO :: EvStream t (IO ()) -> Behavior t ()
debugIO = void . unsafePlan

force :: EvStream t a -> EvStream t a
force = unsafeIOMap . fmap (\x -> return $! x)

class (Functor f) => FilterFunctor f where
  filterMap :: (a -> Maybe b) -> f a -> f b
  {-
  filterMap Just = id
  filterMap f . filterMapt g = filterMap (f >=> g)
  filterMap (Just . f) = fmap f
  -}

instance FilterFunctor [] where
  filterMap _ [] = []
  filterMap f (x:xs) = case f x of
                        Nothing -> filterMap f xs
                        Just b -> b : filterMap f xs


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
instance FilterFunctor (EvStream t) where
  filterMap f = catMaybeEs . fmap f

catMaybes :: (FilterFunctor f) => f (Maybe a) -> f a
catMaybes = filterMap id

ffilter :: (FilterFunctor f) => (a -> Bool) -> f a -> f a
ffilter p = filterMap (\v -> boolToMaybe (p v) v)
  where
    boolToMaybe b v = if b then Just v else Nothing

data Updates t s = Updates (s -> (Updates t s, EvStream t (s -> s)))
                 | NoUpdate

updater :: (s -> EvStream t (s -> s)) -> Updates t s
updater f = Updates ((\e -> (NoUpdate, e)) . f)

instance Monoid (Updates t s) where
  mempty = NoUpdate
  mappend NoUpdate u = u
  mappend u NoUpdate = u
  mappend (Updates f) u2 = Updates ((\(u, e) -> (u <> u2, e)) . f)

runUpdates :: s -> Updates t s -> Behavior t (Reactive t s)
runUpdates iv updates = do
  rec
    let ups = switch $ fmap (flip updateEvStream updates) rState
    rState <- foldEs (\s f -> f s) iv ups

  return rState

updateEvStream :: s -> Updates t s -> EvStream t (s -> s)
updateEvStream _ NoUpdate = never
updateEvStream state (Updates f) = nextFire
  where
    (next, evs) = f state
    nextEvs = updateEvStream state next
    nextFire = mergefEs const (coincidence $ fmap (\fs -> updateEvStream (fs state) next) evs) (mergefEs const evs nextEvs)
