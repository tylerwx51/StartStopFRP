import Control.StartStop.Gloss
import Control.StartStop.Core
import Control.StartStop.Lib
import Graphics.Gloss

main =  runGlossHoldIO (InWindow "FDSLK" (500,500) (0, 0)) white 10 (\e _ -> liftHold (foldEs' (\v _ -> v+1) e 0) >>= \bc -> return $ fmap (text . show) bc)
