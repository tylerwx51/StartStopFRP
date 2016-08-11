 {-# LANGUAGE RecursiveDo, DoAndIfThenElse, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}
module Control.StartStop.Lib where

import Control.StartStop.Core
import Control.Monad


{- Starts the given hold, if its returned value is not Nothing. This is
   equivalent to "catMabyeEs . startOnFire".
-}
pull :: EvStream t (Hold t (Maybe a)) -> EvStream t a
pull = catMaybes . startOnFire

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
gate :: Behavior t Bool -> EvStream t a -> EvStream t a
gate b es = pull $ flip fmap es $ \v -> do
  p <- sample b
  return $ if p then Just v else Nothing

{- "snapshots b e" fires whenever e fires with the value of b when e fires.-}
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
    let ups = f <$> b <@> es
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
