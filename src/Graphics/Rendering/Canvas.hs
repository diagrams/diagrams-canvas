{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module Graphics.Rendering.Canvas
  ( Render(..)
  , doRender

  , newPath
  , moveTo
  , relLineTo
  , relCurveTo
  , arc
  , closePath
  , stroke
  , fill
  , transform
  , save
  , restore
  , translate
  , scale
  , rotate
  , strokeColor
  , fillColor
  , lineWidth
  , lineCap
  , lineJoin
  , globalAlpha
  , withStyle
  ) where

import           Control.Applicative((<$>))
import           Control.Arrow ((***))
import           Control.Monad.State
import           Data.NumInstances ()
import           Data.Word(Word8)
import           Diagrams.Attributes(Color(..),LineCap(..),LineJoin(..))
import qualified Graphics.Blank as C

type RGBA = (Double, Double, Double, Double)

data DrawState = DS
                 { dsPos :: (Float,Float)
                 , dsFill :: RGBA
                 , dsStroke :: RGBA
                 , dsCap :: LineCap
                 , dsJoin :: LineJoin
                 , dsWidth :: Float
                 , dsAlpha :: Float
                 , dsTransform :: (Float,Float,Float,Float,Float,Float)
                 } deriving (Eq)

emptyDS :: DrawState
emptyDS = DS 0 (0,0,0,1) 0 LineCapButt LineJoinMiter 0 1 (1,0,0,1,0,0)

data RenderState = RS
                   { drawState :: DrawState
                   , saved :: [DrawState]
                   }

emptyRS :: RenderState
emptyRS = RS emptyDS []

newtype Render m = Render { runRender :: StateT RenderState C.Canvas m }
  deriving (Functor, Monad, MonadState RenderState)

doRender :: Render a -> C.Canvas a
doRender r = evalStateT (runRender r) emptyRS

canvas :: C.Canvas a -> Render a
canvas = Render . lift

move :: (Float,Float) -> Render ()
move p = modify $ \rs@(RS{..}) -> rs { drawState = drawState { dsPos = p } }

setDS :: DrawState -> Render ()
setDS d = modify $ (\rs -> rs { drawState = d })

saveRS :: Render ()
saveRS = modify $ \rs@(RS{..}) -> rs { saved = drawState : saved }

restoreRS :: Render ()
restoreRS = modify go
  where
    go rs@(RS{saved = d:ds}) = rs { drawState = d, saved = ds }
    go rs = rs

at :: Render (Float,Float)
at = (dsPos . drawState) <$> get

newPath :: Render ()
newPath = canvas $ C.beginPath ()

closePath :: Render ()
closePath = canvas $ C.closePath ()

arc :: Double -> Double -> Double -> Double -> Double -> Render ()
arc a b c d e = canvas $ C.arc (realToFrac a, realToFrac b, realToFrac c, realToFrac d, realToFrac e,True)

moveTo :: Double -> Double -> Render ()
moveTo x y = do
  let x' = realToFrac x
      y' = realToFrac y
  canvas $ C.moveTo (x', y')
  move (x', y')

relLineTo :: Double -> Double -> Render ()
relLineTo x y = do
  p <- at
  let p' = p + (realToFrac x, realToFrac y)
  canvas $ C.lineTo p'
  move p'

relCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
relCurveTo ax ay bx by cx cy = do
  p <- at
  let [(ax',ay'),(bx',by'),(cx',cy')] = map ((p +) . (realToFrac *** realToFrac))
                                          [(ax,ay),(bx,by),(cx,cy)]
  canvas $ C.bezierCurveTo (ax',ay',bx',by',cx',cy')
  move (cx',cy')

stroke :: Render ()
stroke = do

  -- From the HTML5 canvas specification regarding line width:
  --
  --   "On setting, zero, negative, infinite, and NaN values must be
  --   ignored, leaving the value unchanged; other values must change
  --   the current value to the new value.
  --
  -- Hence we must implement a line width of zero by simply not
  -- sending a stroke command.

  w <- gets (dsWidth . drawState)
  when (w > 0) (canvas $ C.stroke ())

fill :: Render ()
fill = canvas $ C.fill ()

save :: Render ()
save = saveRS >> canvas (C.save ())

restore :: Render ()
restore = restoreRS >> canvas (C.restore ())

byteRange :: Double -> Word8
byteRange d = floor (d * 255)

showColorJS :: (Color c) => c -> String
showColorJS c = concat
    [ "rgba("
    , s r, ","
    , s g, ","
    , s b, ","
    , show a
    , ")"
    ]
  where s = show . byteRange
        (r,g,b,a) = colorToRGBA c

setDSWhen :: (DrawState -> DrawState) -> Render () -> Render ()
setDSWhen f r = do
  d <- drawState <$> get
  let d' = f d
  when (d /= d') (setDS d' >> r)

transform :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
transform ax ay bx by tx ty = setDSWhen
                              (\ds -> ds { dsTransform = vs })
                              (canvas $ C.transform vs)
    where vs = (realToFrac ax,realToFrac ay,realToFrac bx,realToFrac by,realToFrac tx,realToFrac ty)

strokeColor :: (Color c) => c -> Render ()
strokeColor c = setDSWhen
                (\ds -> ds { dsStroke = colorToRGBA c})
                (canvas $ C.strokeStyle (showColorJS c))

fillColor :: (Color c) => c -> Render ()
fillColor c = setDSWhen
              (\ds -> ds { dsFill = colorToRGBA c })
              (canvas $ C.fillStyle (showColorJS c))

lineWidth :: Double -> Render ()
lineWidth w = setDSWhen
              (\ds -> ds { dsWidth = w' })
              (canvas $ C.lineWidth w')
  where w' = realToFrac w

lineCap :: LineCap -> Render ()
lineCap lc = setDSWhen
             (\ds -> ds { dsCap = lc })
             (canvas $ C.lineCap (fromLineCap lc))

lineJoin :: LineJoin -> Render ()
lineJoin lj = setDSWhen
              (\ds -> ds { dsJoin = lj })
              (canvas $ C.lineJoin (fromLineJoin lj))

fromLineCap :: LineCap -> String
fromLineCap LineCapRound  = show "round"
fromLineCap LineCapSquare = show "square"
fromLineCap _             = show "butt"

fromLineJoin :: LineJoin -> String
fromLineJoin LineJoinRound = show "round"
fromLineJoin LineJoinBevel = show "bevel"
fromLineJoin _             = show "miter"

globalAlpha :: Double -> Render ()
globalAlpha a = setDSWhen
                (\ds -> ds { dsAlpha = a' })
                (canvas $ C.globalAlpha a')
  where a' = realToFrac a

-- TODO: update the transform's state for translate, scale, and rotate
translate :: Double -> Double -> Render ()
translate x y = canvas $ C.translate (realToFrac x,realToFrac y)

scale :: Double -> Double -> Render ()
scale x y = canvas $ C.scale (realToFrac x,realToFrac y)

rotate :: Double -> Render ()
rotate t = canvas $ C.rotate (realToFrac t)

withStyle :: Render () -> Render () -> Render () -> Render ()
withStyle t s r = do
  save
  r >> t >> s
  stroke
  fill
  restore
