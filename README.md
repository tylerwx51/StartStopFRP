# StartStopFRP
A behavior based frp system

Semantically

Behavior t a :: Time -> a
EvStream t a :: [(Time, a)]

in english

"Behavior t a" are values that change over time. Examples of this could be a (Behavior t String) for what words are currently displayed
on the screen right now, or a (Behavior t (Int, Int)) for the location of your mouse.

"EvStream t a" are values that happen at one point in time. Such as when you click your mouse, or when you press a key on your keyboard.

## Behaviors are a Functor

(fmap f b) creates a behavior whos value is the value of b applied with f.

## EvStreams are a Functor

(fmap f ev) creates a Event Stream that fires whenever ev fires with a value of ev applied to f.

## Behaviors are an Applicative

The pure function returns a constant behavior, so (pure 3) would produce a behavior whos value is always 3.

(bf <*> ba) creates a behavior whos current value is the current value of ba applied to the current value of bf.

## Behaviors are a Monad

Return is the same as pure. So (return x) a constant behavior whos value is always x.

## Examples:

![](https://github.com/tylerwx51/StartStopFRP/blob/master/clickexample.gif)

```
clickExample :: EvStream t [Event] -> Hold t (Behavior t Picture)
clickExample ev = do
  -- filter ev down to only the first mouse click event.
  let mouseClickEvs = filterMap (\xs -> case filterMap isMouseClickEvent xs of
                                          [] -> Nothing
                                          (x:_) -> Just x) ev

  -- creates a behavior with a value of the last clicks position
  bLastMouseClick <- holdEs mouseClickEvs (0,0)

  -- Takes a position and makes a Picture of text with the value of the position
  let positionToText = translate (negate 240) (negate 240) . scale 0.4 0.4 . text . show

  -- return a behavior whos current value is the current screen to draw.
  return $ fmap positionToText bLastMouseClick
```

![](https://github.com/tylerwx51/StartStopFRP/blob/master/mousetracker.gif)

```
-- Takes a list of (x,y) corrdenates and produces a Picture which is a plot.
drawPlot :: [(Float, Float)] -> Picture
drawPlot points = color (Gloss.black) . (Gloss.line) $ shiftedPoints
  where
    max_x = maximum $ fmap fst points
    min_x = minimum $ fmap fst points
    shiftedPoints = fmap (\(x,y) -> (x - max_x, y)) points

-- This takes the clock and a behavior, it then produces a behaivor whos value is a list of samples of b.
holdLastNSecs :: Float -> EvStream t Float -> Behavior t a -> Hold t (Behavior t [(Float, a)])
holdLastNSecs holdTime clock b = foldEs (\vs (t, v) -> (t, v) : filter ((> t - holdTime) . fst) vs) (flip (,) <$> b <@> clock) []

mouseTrackerExample :: EvStream t Float -> EvStream t [Event] -> Hold t (Behavior t Picture)
mouseTrackerExample clock ev = do
  -- behavior whos value is the position of the mouse
  bMousePos <- holdEs (filterMap (\xs -> case filterMap isMouseChange xs of
                                      [] -> Nothing
                                      (x:_) -> Just x) ev) (0,0)

  -- a behavior whos value is a list of samples of the mouses position for the last 5 seconds
  bMousePosData <- holdLastNSecs 5 clock (fmap snd bMousePos)

  return $ fmap (translate 0 0 . scale 50 1 . drawPlot) bMousePosData
```

![](https://github.com/tylerwx51/StartStopFRP/blob/master/mousetrail.gif)

```
holdLastNSecs :: Float -> EvStream t Float -> Behavior t a -> Hold t (Behavior t [(Float, a)])
holdLastNSecs holdTime clock b = foldEs (\vs (t, v) -> (t, v) : filter ((> t - holdTime) . fst) vs) (flip (,) <$> b <@> clock) []

decayColorLine :: [(Float, (Float, Float))] -> Picture
decayColorLine vs = foldl' (flip (<>)) mempty $ fmap (\(t, (x, y)) -> color (timeColor t) $ translate x y $ circleSolid 10) $ vs
  where
    minTime = minimum $ fmap fst vs
    maxTime = maximum $ fmap fst vs

    timeDif = maxTime - minTime
    timeRed t = (t - minTime - 0.1) / (timeDif + 0.1)
    timeBlue t = (maxTime - t) / (timeDif + 0.05)
    timeColor t = makeColor (timeRed t) (timeBlue t) 0 1

    pair = zipWith (\(t, pos1) (_, pos2) -> (t, [pos1, pos2])) vs (drop 1 vs)

mouseTrailExample :: EvStream t Float -> EvStream t [Event] -> Hold t (Behavior t Picture)
mouseTrailExample clock ev = do
  bMousePos <- holdEs (filterMapEs (\xs -> case filterMap isMouseChange xs of
                                      [] -> Nothing
                                      (x:_) -> Just x) ev) (0,0)
  bTrail <- holdLastNSecs 1.2 clock bMousePos

  return $ fmap decayColorLine bTrail
```
