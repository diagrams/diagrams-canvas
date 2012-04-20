{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module Graphics.Rendering.Canvas
  ( Render(..)
  , renderWith
  , withJSSurface
  , withHTMLSurface
  , newPath
  , moveTo
  , lineTo
  , curveTo
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
  , withStyle
  ) where

import Diagrams.Attributes(Color(..),LineCap(..),LineJoin(..))
import Diagrams.TwoD(R2(..))
import Control.Monad.State
import Control.Applicative((<$>))
import Data.List(intersperse)
import Data.Word(Word8)
import Data.Monoid
import Data.NumInstances
import qualified Graphics.Blank as C

type RGBA = (Double, Double, Double, Double)

data DrawState = DS
                 { dsPos :: R2
                 , dsFill :: RGBA
                 , dsStroke :: RGBA
                 , dsCap :: LineCap
                 , dsJoin :: LineJoin
                 , dsWidth :: Float
                 , dsTransform :: (Float,Float,Float,Float,Float,Float)
                 } deriving (Eq)

emptyDS :: DrawState
emptyDS = DS 0 (0,0,0,1) 0 LineCapButt LineJoinMiter 0 []

data RenderState = RS
                   { drawState :: DrawState
                   , saved :: [DrawState]
                   }

emptyRS :: RenderState
emptyRS = RS emptyDS [] mempty

newtype Render m = Render { runRender :: StateT RenderState C.Canvas m }
  deriving (Functor, Monad, MonadState RenderState)

move :: R2 -> Render ()
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

at :: Render R2
at = (dsPos . drawState) <$> get

newPath :: Render ()
newPath = lift $ C.beginPath ()

closePath :: Render ()
closePath = lift $ C.closePath ()

arc :: Double -> Double -> Double -> Double -> Double -> Render ()
arc a b c d e = lift $ C.arc (a,b,c,d,e,True)

moveTo :: Double -> Double -> Render ()
moveTo x y = do
  lift $ C.moveTo (x,y)
  move (x,y)

relLineTo :: Double -> Double -> Render ()
relLineTo x y = do
  p <- at
  let p'@(x',y') = p + (x,y)
  lift $ C.lineTo p'
  move p'

relCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
relCurveTo ax ay bx by cx cy = do
  p <- at
  let [(ax',ay'),(bx',by'),(cx',cy')] = map (p +) [(ax,ay),(bx,by),(cx,cy)]
  lift $ C.bezierCurveTo (ax',ay',bx',by',cx',cy')
  move (cx',cy')

stroke :: Render ()
stroke = lift $ C.stroke ()

fill :: Render ()
fill = lift $ C.fill ()

save :: Render ()
save = saveRS >> lift $ C.save ()

restore :: Render ()
restore = restoreRS >> lift $ C.restore ()

byteRange :: Double -> Word8
byteRange d = floor (d * 255)

showColorJS :: (Color c) => c -> String
showColorJS c = concat
    [ "\"rgba("
    , s r, ","
    , s g, ","
    , s b, ","
    , show a
    , ")\""
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
                              (lift $ C.transform vs)
    where vs = (ax,ay,bx,by,tx,ty)

strokeColor :: (Color c) => c -> Render ()
strokeColor c = setDSWhen
                (\ds -> ds { dsStroke = colorToRGBA c})
                (lift $ C.strokeStyle (showColorJS c))

fillColor :: (Color c) => c -> Render ()
fillColor c = setDSWhen
              (\ds -> ds { dsFill = colorToRGBA c })
              (lift $ C.fillStyle (showColorJS c))

lineWidth :: Float -> Render ()
lineWidth w = setDSWhen
              (\ds -> ds { dsWidth = w })
              (lift $ C.lineWidth w)

lineCap :: LineCap -> Render ()
lineCap lc = setDSWhen
             (\ds -> ds { dsCap = lc })
             (lift $ C.lineCap (fromLineCap lc))

lineJoin :: LineJoin -> Render ()
lineJoin lj = setDSWhen
              (\ds -> ds { dsJoin = lj })
              (lift $ C.lineJoin (fromLineJoin lj))

fromLineCap :: LineCap -> String
fromLineCap LineCapRound  = show "round"
fromLineCap LineCapSquare = show "square"
fromLineCap _             = show "butt"

fromLineJoin :: LineJoin -> String
fromLineJoin LineJoinRound = show "round"
fromLineJoin LineJoinBevel = show "bevel"
fromLineJoin _             = show "miter"

-- TODO: update the transform's state for translate, scale, and rotate
translate :: Double -> Double -> Render ()
translate x y = lift $ C.translate (x,y)

scale :: Double -> Double -> Render ()
scale x y = lift $ C.scale (x,y)

rotate :: Double -> Render ()
rotate t = lift $ C.rotate t

withStyle :: Render () -> Render () -> Render () -> Render ()
withStyle t s r = do
  save
  r >> t >> s
  stroke
  fill
  restore
