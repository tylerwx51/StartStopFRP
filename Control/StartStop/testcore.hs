{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}

module Control.StartStop.TestCore where

import Control.StartStop.Pure
import Control.StartStop.Run
import Control.StartStop.Class

type TestFails = [String]
testNetwork :: forall a b . (Show b, Eq b) => [(Integer, a)] -> (forall t . (StartStop t) => EvStream t a -> Behavior t (Reactive t b)) -> IO TestFails
testNetwork eventStream f = do
  let pureBehavior = runPure eventStream f
      tick2stream :: (FilterFunctor f) => f Integer -> f a
      tick2stream = filterMap isInStream
      coreBehavior = runACoreBehavior (f . tick2stream)
      isInStream = flip lookup eventStream
      lastTime = 2 + maximum (fmap fst eventStream)
  nextV <- coreBehavior
  let loop t = do
        let pureV = pureBehavior t
        coreV <- nextV
        if pureV == coreV
        then if t < lastTime
             then loop (t + 1)
             else return []
        else return ["@Time " ++ show t ++ ": Pure Value \n\t" ++ show pureV ++ "\ndoes not equal core value \n\t" ++ show coreV]

  loop 0

simpleHoldTest :: (StartStop t) => EvStream t String -> Behavior t (Reactive t String)
simpleHoldTest = holdEs "___"
