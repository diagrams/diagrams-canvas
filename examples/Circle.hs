{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Canvas.CmdLine

main = mainWith (circle 300
              # lw thick 
              # fc red
              # frame 10 :: Diagram Canvas R2)
