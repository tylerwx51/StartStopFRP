{-# LANGUAGE Rank2Types, GADTs #-}
module Control.StartStop.Free where

import Control.Monad
import Control.Monad.Fix

{-
- I will refer to anything with the type (forall x . f x -> m x) as a interpeter from f to m.
- k, will be an arbetrary interperter from f to m.
-}

{-
-- The Free type will take an interpeter from f to some monad m, then produce a interpeter from (Free f) to m.
-- Free f a will be a monad as well. We do not need to know anything about f.
-}
data Free f a = Free { foldFreeImpl :: forall m. (Monad m) => (forall x . f x -> m x) -> m a }

{-
-- \free -> Free $ \k -> foldFree k free = \(Free fimpl) -> Free $ \k -> fimpl k
--                                       = \(Free fimpl) -> Free fimpl
--                                       = id
-- \free -> Free $ \k -> foldFree k free = id
-}

{- foldFree will take an interpeter from f to some monad m, then produce a interpeter from (Free f) to m.
-}
foldFree :: (Monad m) => (forall x . f x -> m x) -> Free f a -> m a
foldFree interp free = foldFreeImpl free interp

hoistFree :: (forall x . f x -> g x) -> Free f a -> Free g a
hoistFree interp = foldFree (liftF . interp)

liftF :: f a -> Free f a
liftF fa = Free $ \k -> k fa

instance Functor (Free f) where
  fmap f free = Free $ \k -> fmap f (foldFree k free)

  {- (fmapF g . fmapF f) free = fmapF g (fmapF f free)
  --                          = fmapF g (Free $ \k -> fmapM f (foldFree k free))
  --                          = Free $ \k1 -> fmapM g (foldFree k1 $ Free $ \k2 -> fmapM f (foldFree k2 free))
  --                          = Free $ \k1 -> fmapM g (fmapM f (foldFree k1 free))
  --                          = Free $ \k -> fmapM (g . f) (foldFree k free)
  --                          = fmapF (g . f) free
  -- fmapF g . fmapF f = fmapF (g . f)
  --
  -- fmapF id free = Free $ \k -> fmapM id (foldFree k free)
  --              = Free $ \k -> foldFree k free
  --              = free
  -- fmapF id = id
  -}

instance Applicative (Free f) where
  pure = return
  (<*>) = ap

instance Monad (Free f) where
  return x = Free $ \_ -> return x
  m >>= f = joinFree (fmap f m)
  {-
  -- fmapF f . returnF = \v -> fmapF f (returnF v)
  --                   = \v -> fmapF f (Free $ \_ -> returnM v)
    --                 = \v -> Free $ \k -> fmapM f (foldFree k (Free $ \_ -> returnM v))
    --                 = \v -> Free $ \k -> fmapM f (returnM v)
    --                 = \v -> Free $ \_ -> returnM (f v)
    --                 = \v -> returnF (f v)
    --                 = return . f
  -- fmapF f . returnF = returnF . f
  --
  -- joinF . fmapF returnF = \free -> Free $ \k -> (joinM . foldFree k . fmapF (foldFree k) . fmapF returnF) free
    --                     = \free -> Free $ \k -> (joinM . foldFree k . fmapF (foldFree k . returnF)) free
    --                     = \free -> Free $ \k -> (joinM . foldFree k . fmapF returnM) free
    --                     = \free -> Free $ \k -> (joinM . fmapM returnM . foldFree k) free
    --                     = \free -> Free $ \k -> foldFree k free
    --                     = id
  --  join . fmap return = id
  --
  -- joinF . returnF = \v -> Free $ \k -> (joinM . foldFree k . fmapF (foldFree k) . returnF) v
    --               = \v -> Free $ \k -> (joinM . foldFree k . returnF . foldFree k) v
    --               = \v -> Free $ \k -> (joinM . returnM . foldFree k) v
    --               = \v -> Free $ \k -> foldFree k v
    --               = id
  -- joinF . returnF = id
  --
  -- joinF . fmapF joinF = \free -> Free $ \k -> (joinM . foldFree k . fmapF (foldFree k) . fmapF joinF) free
    --                   = \free -> Free $ \k -> (joinM . foldFree k . fmapF (foldFree k . joinF)) free
    --                   = \free -> Free $ \k -> (joinM . foldFree k . fmapF (joinM . foldFree k . fmapF (foldFree k))) free
    --                   = \free -> Free $ \k -> (joinM . fmapM joinM . foldFree k . fmapF (foldFree k . fmapF (foldFree k))) free
    --                   = \free -> Free $ \k -> (joinM . joinM . foldFree k . fmapF (foldFree k . fmapF (foldFree k))) free
    -- joinF . joinF = \free -> Free $ \k -> (joinM . fmapM (foldFree k) . foldFree k) (Free $ \k2 -> (joinM . fmapM (foldFree k2) . foldFree k2) free)
    --               = \free -> Free $ \k -> (joinM . fmapM (foldFree k) . joinM . fmapM (foldFree k) . foldFree k) free
    --               = \free -> Free $ \k -> (joinM . joinM . fmapM (fmapM (foldFree k)) . fmapM (foldFree k) . foldFree k)
    --               = \free -> Free $ \k -> (joinM . joinM . foldFree k . fmapF (foldFree k . fmapF (foldFree k))) free
  -- joinF . joinF = joinF . fmapF joinF

  -- fmapF f . joinF = \free -> Free $ \k -> (fmapM f . foldFree k) (Free $ \k2 -> (joinM . fmapM (foldFree k) . foldFree k) free)
    --               = \free -> Free $ \k -> (fmapM f . joinM . fmapM (foldFree k) . foldFree k) free
    --
    -- joinF . fmapF (fmapF f) = \free -> Free $ \k -> (joinM . fmapM (foldFree k) . foldFree k . fmapF (fmapF f)) free
    --                         = \free -> Free $ \k -> (joinM . fmapM (foldFree k) . fmapM (fmapF f) . foldFree k) free
    --                         = \free -> Free $ \k -> (joinM . fmapM (fmapM f . foldFree k) . foldFree k) free
    --                         = \free -> Free $ \k -> (fmapM f . joinM . fmapM (foldFree k) . foldFree k) free
  -- fmapF f . joinF = joinF . fmapF (fmapF f)
  -}

joinFree :: Free f (Free f a) -> Free f a
joinFree free = Free $ \k -> (join . foldFree k . fmap (foldFree k)) free

data FreeFix f a = FreeFix { foldFreeFixImpl :: forall m. (MonadFix m) => (forall x . f x -> m x) -> m a }

foldFreeFix :: (MonadFix m) => (forall x . f x -> m x) -> FreeFix f a -> m a
foldFreeFix interp free = foldFreeFixImpl free interp

hoistFreeFix :: (forall x . f x -> g x) -> FreeFix f a -> FreeFix g a
hoistFreeFix interp = foldFreeFix (liftFF . interp)

liftFF :: f a -> FreeFix f a
liftFF fa = FreeFix $ \k -> k fa

instance Functor (FreeFix f) where
  fmap f (FreeFix trans) = FreeFix $ \k -> fmap f (trans k)

instance Applicative (FreeFix f) where
  pure = return
  (<*>) = ap

instance Monad (FreeFix f) where
  return x = FreeFix $ \_ -> return x
  m >>= f = joinFreeFix (fmap f m)

joinFreeFix :: FreeFix f (FreeFix f a) -> FreeFix f a
joinFreeFix free = FreeFix $ \k -> (join . foldFreeFix k . fmap (foldFreeFix k)) free

instance MonadFix (FreeFix f) where
  mfix f = FreeFix $ \k -> mfix (foldFreeFix k . f)
{- m >>=F f = FreeFix $ \k -> foldFreeFix k m >>=M foldFreeFix k . f
-- mfixF (returnF . h) = FreeFix $ \k -> mfixM (foldFreeFix k . returnF . h)
  --                   = FreeFix $ \k -> mfixM (returnM . h)
  --                   = FreeFix $ \k -> returnM (fix h)
  --                   = returnF (fix h)
-- mfixF (returnF . h) = returnF (fix h)
--
-- mfixF (\x -> a >>=F \y -> f x y) = FreeFix $ \k -> mfixM (foldFreeFix k . (\x -> a >>=F \y -> f x y))
--                                  = FreeFix $ \k -> mfixM (\x -> foldFreeFix k (a >>=F \y -> f x y))
--                                  = FreeFix $ \k -> mfixM (\x -> foldFreeFix k a >>=M \y -> foldFreeFix k (f x y))
--                                  = FreeFix $ \k -> mfixM (\x -> foldFreeFix k a >>=M \y -> foldFreeFix k (f x y))
--                                  = FreeFix $ \k -> foldFreeFix k a >>=M \y -> mfixM (\x -> foldFreeFix k (f x y))
--
--
-- a >>=F \y -> mfixF (\x -> f x y) = FreeFix $ \k -> foldFreeFix k a >>=M foldFreeFix k . (\y -> mfixF (\x -> f x y))
--                                  = FreeFix $ \k -> foldFreeFix k a >>=M \y -> mfixM (\x -> foldFreeFix k (f x y))
--
-- mfixF (\x -> a >>=F \y -> f x y) = a >>=F \y -> mfixF (\x -> f x y)
--
-- mfixF (fmapF h . f) = FreeFix $ \k -> mfixM (foldFreeFix k . fmapF h . f)
  --                   = FreeFix $ \k -> mfixM (fmapM h . foldFreeFix k . f)
  --                   = FreeFix $ \k -> fmapM h $ mfixM (foldFreeFix k . f . h)
  -- fmapF h (mfixF (f . h)) = FreeFix $ \k -> (fmapM h . foldFreeFix k) (mfixF (f . h))
  --                         = FreeFix $ \k -> fmapM h $ mfixM (foldFreeFix k . f . h)
-- mfixF (fmapF h . f) = fmapF h (mfixF (f . h)), for strict h
--
-- mfixF (\x -> mfixF (\y -> f x y)) = FreeFix $ \k -> mfixM (foldFreeFix k . (\x -> mfixF (\y -> f x y)))
  --                                 = FreeFix $ \k -> mfixM (\x -> foldFreeFix k (mfixF (\y -> f x y)))
  --                                 = FreeFix $ \k -> mfixM (\x -> mfixM (foldFreeFix k . (\y -> f x y)))
  --                                 = FreeFix $ \k -> mfixM (\x -> mfixM (\y -> foldFreeFix k (f x y)))
  --                                 = FreeFix $ \k -> mfixM (\x -> foldFree k (f x x))
  --                                 = mfixF (\x -> f x x)
-- mfixF (\x -> mfixF (\y -> f x y)) = mfixF (\x -> f x x)
-}
