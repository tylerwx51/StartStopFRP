module Control.StartStop.Gloss where

import Control.StartStop.Core
import Control.StartStop.Lib
import Control.StartStop.EvPrim
import Control.StartStop.RunHold

import Control.Monad.IO.Class

import Data.IORef

import Graphics.Gloss.Interface.IO.Game

runGlossHoldIO :: Display -> Color -> Int -> (EvStream t Float -> EvStream t Event -> PlanHold t (Behavior t Picture)) -> IO ()
runGlossHoldIO displayMode bgColor fps bPicture = do
  actionsRef <- newIORef []
  pictureRef <- newIORef undefined

  timeTriggerRef <- newIORef undefined
  eventTriggerRef <- newIORef undefined
  renderTriggerRef <- newIORef undefined

  let pl = do
        (timeTrigger, clock) <- callbackStream
        (eventTrigger, events) <- callbackStream
        --(renderTrigger, renderEv) <- callbackStream

        liftIO $ writeIORef timeTriggerRef timeTrigger
        liftIO $ writeIORef eventTriggerRef eventTrigger
        --liftIO $ writeIORef renderTriggerRef renderTrigger

        bPic <- bPicture (fmap head clock) (fmap head events)
        let renderPic = snapshots bPic clock
        planEs $ flip fmap renderPic $ \pic -> liftIO $ writeIORef pictureRef pic

        inital <- liftHold $ sample bPic
        liftIO $ writeIORef pictureRef inital
        return ()

      runActions = do
        actions <- readIORef actionsRef
        writeIORef actionsRef []
        sequence_ actions

  initPlanHold (\a -> modifyIORef actionsRef (\as -> as ++ [a])) pl
  playIO displayMode
         bgColor
         fps
         ()
         (\() -> readIORef pictureRef >>= \p -> return $! p)
         (\event _ -> readIORef eventTriggerRef >>= \t -> t event)
         (\time _ -> readIORef timeTriggerRef >>= \t -> t time >> runActions)
