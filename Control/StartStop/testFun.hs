module Control.StartStop.TestFun where

import Control.StartStop.Core
import Control.StartStop.Lib
import Control.StartStop.EvPrim
import Control.Monad.Reader
import Data.IORef

main = testHold 100000 test10

testPlanHold n = do
  actionsRef <- newIORef []
  clockTriggerRef <- newIORef undefined
  sampleRef <- newIORef ""

  initPlanHold (\a -> modifyIORef actionsRef (\as -> as ++ [a])) $ do
    (trigger, clock) <- callbackStream
    liftIO $ writeIORef clockTriggerRef trigger
    b <- test12 $ fmap head clock
    let r = startOnFire $ sampleAfter b <$ clock
    planEs $ fmap (writeIORef sampleRef) r
    return ()

  let loop i = do
        trigger <- readIORef clockTriggerRef
        trigger (i + 1)

        actions <- readIORef actionsRef
        sequence_ actions

        s <- readIORef sampleRef
        print (i, s)
        unless (i > n) $ loop (i + 1)

  loop 0
