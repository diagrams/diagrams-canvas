{-# LANGUAGE NoMonomorphismRestriction #-}


import Diagrams.Prelude
import Diagrams.Backend.Canvas.CmdLine


pt = circle 0.1 # fc red # lw none
t1 = pt <> topLeftText "top left" # fontSizeL 1 <> rect 8 1 # lwO 1
t2 = pt <> baselineText "baseline" # fontSizeO 20 <> rect 8 1 # lwO 1
t3 = pt <> alignedText 0.76 0.5  "(0.7, 0.5)" # fontSizeO 20 <> rect 8 1 # lwO 1
t4 = pt <> text  "Hello world!" # fontSizeN 0.05 <> rect 8 1 # lwO 1
d1 =/= d2 = d1 === strutY 2 === d2

-- example :: Diagram B R2
-- example = t1 =/= t2 =/= t3 =/= t4

eff = text "M" <> square 1

example :: Diagram Canvas
example = hcat
  [eff, eff # scale 2, eff # scaleX 2, eff # scaleY 2, eff # rotateBy (1/12)]
    # fontSizeL 1

main = mainWith example
