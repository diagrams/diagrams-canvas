{- 1. Compile
   2. Run with a size parameter, e.g.

      ./Sierpinski -w 400

   3. Visit  localhost:3000  in your browser
-}

{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude
import Diagrams.Backend.Canvas.CmdLine

sierpinski 1 = eqTriangle 1
sierpinski n =     s
                  ===
               (s ||| s) # centerX
  where s = sierpinski (n-1)

example = pad 1.1 $ sierpinski 7 # centerXY # lw 0 # fc black

main = defaultMain example