{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Diagrams.Prelude hiding (dot)
import Diagrams.Backend.Canvas.CmdLine
import Control.Lens ((^.))

theta  = 3/8 @@ turn
alpha  = 7/16 @@ turn
chordX = cos (theta ^. rad)
chordY = sin (theta ^. rad)
midX   = sin (theta ^. rad - pi / 2)
h      = sin (alpha ^. rad)
w      = cos (alpha ^. rad)

dot    = circle 0.03 # lw none # fc red
sircle = circle 1 # lw veryThin # dashingN [0.01, 0.01] 0

back     = arc' 1 (xDir # rotate theta) (negated theta)
back'    = back # reflectX # translateX (2 * chordX)
thetaArc = arc' 0.15 xDir theta # lw veryThin

top   = (1 ^& 0) ~~ (chordX ^& chordY)
bot   = reflectY top
hd    = fc lightgray . lw none . translateX 1 . strokeTrail . glueTrail 
      . mconcat $ [top, back', bot]
chord = lc green . stroke $ (chordX ^& chordY) ~~ (chordX ^& 0)

drawLine :: V2 Double -> Diagram B
drawLine = stroke . fromOffsets . (:[])

r1       = dashingN [0.01, 0.01] 0 . lw veryThin . drawLine $ (1 ^& 0)
r_1      = lc green . reflectX . drawLine $ (1 ^& 0)
rTheta   = lc green . drawLine $ unitX # rotate theta
rj       = lc green . drawLine $ unitX # rotate alpha 
rMid     = lc green . drawLine $ (-midX ^& 0)


-------------------------------------------------------------------------------

text' :: String -> Diagram B
text' s = text s # italic # fontSize (output 13)

aLabel       = text' "a" # translate (0.05 ^& 0.2)
rsinaLabel   = text' "r sin a" # translate (-0.83 ^& (-0.2))
rLabel       = text' "r" # translate (-0.3 ^& (-0.4))
rcosaLabel   = text' "-r cos a" # translate (-0.375 ^& (0.1))
lengthLabel  = text' "l = 2r + r cos a" # translate (0 ^& (-0.7))
jLengthLabel = text' "j = r - sqrt(r^2 - w^2/4)" # translate (0.1 ^& (-0.7))
wLabel       = text' "w/2" # translate (-0.8 ^& (-0.15))
jLabel       = text' "j" # translate (-(0.95) ^& 0.06) 
r_jLabel     = text' "r - j" # translate (-(0.6) ^& 0.06)
hght         = (w ^& 0) ~~ (w ^& h)
jArc         = arc' 1 (xDir # rotate alpha) (pi @@ rad)
jt           = fc lightgray 
             . lc green 
             . translate (w ^& 0) 
             . strokeTrail 
             . closeTrail 
             . mconcat $ [hght, jArc]

aHead :: Diagram B
aHead = aLabel <> rsinaLabel <> rLabel <> rcosaLabel <> lengthLabel
  <> back # lc red 
  <> chord
  <> r1 <> rTheta <> rMid
  <> thetaArc
  <> dot <> sircle <> hd

aJoint :: Diagram B
aJoint = rLabel # translate (-0.25 ^& 0.1) 
      <> wLabel <> jLabel <> r_jLabel <> jLengthLabel
      <> jt
      <> r_1 <> rj
      <> dot <> sircle <> hd

main = mainWith $ (aHead ||| strutX 0.25 ||| aJoint) # frame 0.2
