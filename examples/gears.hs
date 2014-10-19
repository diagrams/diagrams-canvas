{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Lens ((^.))
import Data.Colour.SRGB              (sRGB24read)
import Diagrams.Prelude
import Diagrams.Backend.Canvas
import Graphics.Blank                hiding ((#), rotate, scale)
import Control.Concurrent

blue2, blue3, blue6, blue7 :: Colour Double
[ blue2, blue3, blue6, blue7] =
  map sRGB24read ["#c6dbef", "#9ecae1", "#2171b5", "#08519c"]

wheel :: [Colour Double] -> Diagram B
wheel [] = circle 1 # fc black
wheel cs = wheel' # rotateBy r
  where
    wheel' = mconcat $ zipWith fc cs (iterateN n (rotateBy a) w)
    n = length cs
    a = 1 / fromIntegral n
    w = wedge 1 xDir (a @@ turn) # lw none
    r = 1/4 - 1/(2*fromIntegral n)

planet :: Angle Double -> Diagram B
planet r = circle 0.8 # fc black
        <> wheel (take 12 . cycle $ [blue3, blue6])
         # rotate r
         # lw none

planets :: Angle Double -> Diagram B
planets r
  = atPoints (trailVertices $ square 2) (repeat (planet r)) 
  # rotateBy (1/8) # centerXY

sun :: Angle Double -> Diagram B
sun r = w # rotate ((r^.turn / (1 - sqrt 2)) @@ turn)
  where
    w = circle 0.3 # fc black <> wheel (take 60 . cycle
      $ [ blue2, blue7])
      # scale (sqrt 2 -1)
      # rotateBy (1/8)

solar :: Angle Double -> Diagram B
solar r = bg black . pad 1.1 . centerXY $ sun r <> planets r

main :: IO ()
main = blankCanvas 3000 $ \context -> loop context 0

loop :: DeviceContext -> Double -> IO a
loop context n = do
  send context $ renderDia Canvas (CanvasOptions (mkWidth 500)) 
                                  (solar (n/588 @@ turn))
  threadDelay 2000
  loop context (n + 1)
