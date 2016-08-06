import TryItOut

main = tryItOut $ \clock events -> do
  -- filter ev down to only the first mouse click event.
  let mouseClickEvs = filterMap isMouseClickEvent events

  -- creates a behavior with a value of the last clicks position
  bLastMouseClick <- holdEs mouseClickEvs (0,0)

  -- Takes a position and makes a Picture of text with the value of the position
  let positionToText = translate (negate 240) (negate 240) . scale 0.4 0.4 . text . show

  -- return a behavior whos current value is the current screen to draw.
  return $ fmap positionToText bLastMouseClick
