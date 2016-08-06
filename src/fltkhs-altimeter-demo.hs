module Main where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Data.Either
import Data.IORef
import System.Directory

data AltimeterState = AltimeterState
  {
    alt :: Int
  , drum000 :: Ref PNGImage
  , drum10k :: Ref PNGImage
  , dial :: Ref PNGImage
  , face :: Ref PNGImage
  }


-- Draw pointer with center at xc,yc, radius, and pointer rotary position 'f',
-- where 'f' is a fractional value between 0.0 and 1.0:
--    0.0 -- top position ("12 o'clock")
--    0.5 -- bottom position ("6 o'clock")
--
drawPointer :: Int -> Int -> Int -> Double -> IO ()
drawPointer xc yc radius f =
  let ang = pi + (f * (pi * 2))
      xa = sin ang
      ya = cos ang
      x1 = truncate (fromIntegral xc + xa * fromIntegral radius * 0.40)
      y1 = truncate (fromIntegral yc + ya * fromIntegral radius * 0.40)
      x2 = truncate (fromIntegral xc + xa * fromIntegral radius * 0.75)
      y2 = truncate (fromIntegral yc + ya * fromIntegral radius * 0.75)
      x3 = truncate (fromIntegral xc + xa * fromIntegral radius * 0.96)
      y3 = truncate (fromIntegral yc + ya * fromIntegral radius * 0.96)
      x4 = truncate (fromIntegral xc + xa * fromIntegral radius * 0.98)
      y4 = truncate (fromIntegral yc + ya * fromIntegral radius * 0.98)
      x5 = truncate (fromIntegral xc + xa * fromIntegral radius * 1.00)
      y5 = truncate (fromIntegral yc + ya * fromIntegral radius * 1.00)
  in
    do
     -- Draw black part of needle
     flcLineStyle SolidLineStyle (Just 2) Nothing
     flcSetColor blackColor
     flcLine (Position (X xc) (Y yc)) (Position (X x1) (Y y1))
      -- Draw white part of needle
     flcLineStyle SolidLineStyle (Just 3) Nothing
     flcSetColor (Color 0xdbd6c700)
     flcLine (Position (X x1) (Y y1)) (Position (X x2) (Y y2))

     flcLineStyle SolidLineStyle (Just 8) Nothing
     flcSetColor (Color 0xdbd6c700)
     flcLine (Position (X x2) (Y y2)) (Position (X x3) (Y y3))

     flcLineStyle SolidLineStyle (Just 4) Nothing
     flcSetColor (Color 0xdbd6c700)
     flcLine (Position (X x3) (Y y3)) (Position (X x4) (Y y4))

     flcLineStyle SolidLineStyle (Just 2) Nothing
     flcSetColor (Color 0xdbd6c700)
     flcLine (Position (X x4) (Y y4)) (Position (X x5) (Y y5))

drawAltimeter :: IORef AltimeterState -> Ref Widget -> IO ()
drawAltimeter currentState widget = do
  x <- getX widget
  y <- getY widget
  w <- getW widget
  h <- getH widget
  altimeterState <- readIORef currentState
  flcPushClip (toRectangle (x,y,w,h))
  -- Digits are 25 pixels high
  -- When a digit is 9 -> 0, next digit should start rolling
  -- and be done by the time 0 shows up.
  --
  --       -   -- 0        :
  --       0               -   -- 200
  --       -   -- 25       8
  --       1               -   -- 225
  --       -   -- 50       9
  --       2               -   -- 250
  --       -   -- 75       0
  --       :
  --
  let a = if (alt altimeterState) > 0
          then (alt altimeterState)
          else (alt altimeterState) + 10000
      off = (a `div` 4) `mod` 25
      calculateDelta unit = 253 - ((a `div` unit) `mod` 10) * 25
      d100 = (calculateDelta 100)  - off
      d1000 = (calculateDelta 1000) -
              (if (a `mod` 1000) >= 900 then off else 0)   -- rollover 1k  digit during 100's 9->0
      d10000 = (calculateDelta 10000) -
               (if (a `mod` 1000) >= 9900 then off else 0) -- rollover 10k digit during 1000's 9->0
  -- Draw drum tape numbers first
  draw (drum10k altimeterState)
       (Position (X (x+65+25*0)) (Y (y+115-d10000))) -- +65: offset of drum window from left
  draw (drum000 altimeterState)
       (Position (X (x+65+25*1)) (Y (y+115-d1000)))  -- +115: offset of drum window from top
  draw (drum000 altimeterState)
       (Position (X (x+65+25*2)) (Y (y+115-d100)))   -- 25: height of digits on tape
  -- Draw dial mask over drum tape
  draw (dial altimeterState) (Position (X x) (Y y))

  -- Draw pointer over dial
  let f = (fromIntegral (a `mod` 1000)) / 1000.0
  drawPointer (x+153) (y+150) 115 f
  draw (face altimeterState) (Position (X x) (Y y))
  flcPopClip

altimeterNew :: Rectangle -> IO (IORef AltimeterState, Ref Widget)
altimeterNew bounds = do
  let pngImageCreate path = do
        image <- pngImageNew path
        case image of
          Right i -> return i
          Left _ -> do
            d <- getCurrentDirectory
            error ("Could not find image: " ++ path ++ " in current directory: " ++ d ++
                   "\nPlease run the executable within a directory containing " ++ path ++ ".")
  _drum000 <- pngImageCreate "alt-drumtape.png"
  _drum10k <- pngImageCreate "alt-drumtape-10k.png"
  _dial <- pngImageCreate "alt-dial.png"
  _face <- pngImageCreate "alt-faceplate.png"
  altimeterState <- newIORef (AltimeterState
                              {
                                alt = 0
                              , drum000 = _drum000
                              , drum10k = _drum10k
                              , dial = _dial
                              , face = _face
                              })
  altimeter <- widgetCustom
                 bounds
                 Nothing
                 (drawAltimeter altimeterState)
                 defaultCustomWidgetFuncs
  return (altimeterState, altimeter)

-- Timeout callback to animate pointer for testing
changeAltitudeCallback :: IORef AltimeterState -> IORef Int -> Ref Widget -> IO ()
changeAltitudeCallback currentState delta altimeter = do
  altimeterState <- readIORef currentState
  if (alt altimeterState) >= 14500
    then modifyIORef delta (\current -> current - 5)      -- when reached, change to descending altitude
    else if (alt altimeterState) <= (-2500)
         then modifyIORef delta (\current -> current + 5) -- when reached, change to ascending altitude
         else return ()
  _delta <- readIORef delta
  writeIORef currentState (altimeterState { alt = (alt altimeterState) + _delta })
  redraw altimeter
  FL.repeatTimeout 0.02 (changeAltitudeCallback currentState delta altimeter)

main :: IO ()
main = do
 window <- doubleWindowNew
           (Size (Width 320) (Height 320))
           Nothing
           (Just "Altimeter")
 (altimeterState, g_alt) <- altimeterNew (toRectangle (10,10,300,300))
 showWidget window
 initialDelta <- newIORef 5
 FL.addTimeout 0.02 (changeAltitudeCallback altimeterState initialDelta g_alt)
 _ <- FL.run
 return ()
