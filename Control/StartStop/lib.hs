 {-# LANGUAGE RecursiveDo, DoAndIfThenElse #-}
module Control.StartStop.Lib where

import Control.StartStop.Core
import Control.Monad.Reader
import Control.Monad.Writer.Strict

import Data.IORef

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
    let ups = (\va vb -> seq vb (f va vb)) <$> b <@> es
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
