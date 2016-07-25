import Control.StartStop.Core
import Control.StartStop.Lib
import Control.StartStop.Gloss

import Graphics.Gloss

import Buttons

count :: EvStream t x -> Hold t (Behavior t Int)
count e = foldEs' (\v _ -> v + 1) e 0

main :: IO ()
main = runGlossHoldIO (InWindow "FRP-Zoo" (500,500) (10, 10)) white 10 $ \time events -> liftHold $ do
  let click0 = filterEs (==Click) $ filterMapEs filter0 events
      click5 = filterEs (==Click) $ filterMapEs filter5 events
      click10 = filterEs (==Click) $ filterMapEs filter10 events

      toggle0 = filterEs (==Toggle) $ filterMapEs filter0 events
      toggle5 = filterEs (==Toggle) $ filterMapEs filter5 events
      toggle10 = filterEs (==Toggle) $ filterMapEs filter10 events

  mode0 <- toggle toggle0 True
  mode5 <- toggle toggle5 True
  mode10 <- toggle toggle10 True

  count0 <- foldEs (\v f -> f v) (leftmost (const 0 <$ toggle0) ((+1) <$ gate mode0 click0)) 0
  count5 <- count $ gate mode5 click5
  count10 <- count click10

  {- senario 0 -}
  let toggleDyn0 False = return $ return 0
      toggleDyn0 True = count click0

      dToggle0 = startOnFire $ toggleDyn0 <$> changes mode0

  dCount0 <- switcher count0 dToggle0

  let picture = renderButtons <$> count0 <*> fmap Just dCount0 <*> count5 <*> pure Nothing <*> count10 <*> pure Nothing
  return picture
