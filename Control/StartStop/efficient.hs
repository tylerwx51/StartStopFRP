{-# LANGUAGE ScopedTypeVariables #-}
module Control.StartStop.Efficient where

import Control.StartStop.Core
import Control.Monad.Writer.Strict

{- Typeclass hack for efficiency. I feel like this should be something
-  that can be compiler can check but I don't know how. This typeclass allows for more
-  efficient samples and allows for removing unecissary side effects of events.
-  This feels a little hackish as well.
-}
class BehaviorBased a where
  behaviorBased :: a -> Bool

instance BehaviorBased (Behavior t a) where
  behaviorBased _ = True

instance (BehaviorBased a) => BehaviorBased (EvStream t a) where
  behaviorBased _ = behaviorBased (undefined :: a)

instance BehaviorBased (a -> b) where
  behaviorBased _ = False

instance BehaviorBased (Hold t a) where
  behaviorBased _ = False

instance BehaviorBased (HoldIO t a) where
  behaviorBased _ = False

instance BehaviorBased (PlanHold t a) where
  behaviorBased _ = False

instance BehaviorBased Int where
  behaviorBased _ = False

instance BehaviorBased Char where
  behaviorBased _ = False

instance forall a . (BehaviorBased a) => BehaviorBased [a] where
  behaviorBased _ = behaviorBased (undefined :: a)

instance forall a . (BehaviorBased a) => BehaviorBased (Maybe a) where
  behaviorBased _ = behaviorBased (undefined :: a)

sampleEfficient :: forall t a . (BehaviorBased a) => Behavior t a -> Hold t a
sampleEfficient = sample . pruneBehavior

pruneEvs :: forall t a . (BehaviorBased a) => EvStream t a -> EvStream t a
pruneEvs evs =
  if behaviorBased (undefined :: a)
  then evs
  else case evs of
    Never -> Never
    (EvStream me) -> EvStream $ do
      eInfo <- me
      case eInfo of
        NotFired -> return NotFired
        FiredNow a p -> return $ FiredNow a mempty

pruneBehavior :: forall t a . (BehaviorBased a) => Behavior t a -> Behavior t a
pruneBehavior (BConst a) = BConst a
pruneBehavior b =
  if behaviorBased (undefined :: a)
  then b
  else Behavior $ Hold $ do
    (bi, _) <- lift . runWriterT . unHold . runB $ b
    return bi
