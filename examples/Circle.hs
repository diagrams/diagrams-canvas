{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Canvas.CmdLine

main = mainWith ((square 200 # fc blue <> circle 300)
              # lw thick 
              # fc red
              # frame 10 :: Diagram Canvas)
