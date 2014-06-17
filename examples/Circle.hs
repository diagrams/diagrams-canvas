{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Canvas.CmdLine

main = mainWith (circle 1 :: Diagram Canvas R2)
