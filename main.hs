import Control.StartStop.Gloss
import Control.StartStop.Core
import Control.StartStop.Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.IO.Class

main = runGlossHoldIO (InWindow "FDSLK" (500,500) (10, 10)) white 10 $ \e ev -> do
  bc <- liftHold (foldEs' (\v _ -> v+1) e 0)
  let isQuit (EventKey (Char 'q') Down _ _) = True
      isQuit _ = False

      quitEvStream = filterEs isQuit ev
  planEs $ undefined <$ quitEvStream
  return $ fmap (text . show) bc
