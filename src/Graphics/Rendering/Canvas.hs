{-# LANGUAGE GeneralizedNewtypeDeriving
           , RecordWildCards
           , OverloadedStrings 
           , TemplateHaskell #-}

module Graphics.Rendering.Canvas
  ( RenderM(..)
  , liftC
  , getStyleAttrib
  , runRenderM
  , accumStyle
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
  , fromLineCap
  , fromLineJoin
  ) where

import           Control.Applicative((<$>))
import           Control.Arrow ((***))
import           Control.Lens hiding (transform, (#))
import           Control.Monad.State
import           Data.Default.Class
import           Data.Maybe (fromMaybe)
import           Data.NumInstances ()
import           Data.Word(Word8)
import           Diagrams.Prelude hiding (font, moveTo, arc, stroke, fillColor,
                                          transform)

import           Diagrams.Attributes(Color(..),LineCap(..),LineJoin(..), 
                                     SomeColor(..), colorToSRGBA)
import           Diagrams.Core.Style           (Style)
import           Diagrams.Core.Compile
import           Diagrams.Core.Types
import           Diagrams.Core.Transform  hiding (transform)
import           Diagrams.TwoD.Attributes (Texture(..))
import           Diagrams.TwoD.Types      (R2(..))
import qualified Graphics.Blank as BC
import qualified Data.Text as T
import           Data.Text (Text)
import           Control.Applicative
import qualified Control.Monad.StateStack as SS

data CanvasState = CanvasState { _accumStyle :: Style R2
                               , _csPos :: (Float, Float)}

makeLenses ''CanvasState

instance Default CanvasState where
  def = CanvasState { _accumStyle = mempty
                    , _csPos = (0,0)}

type RenderM a = SS.StateStackT CanvasState BC.Canvas a

liftC :: BC.Canvas a -> RenderM a
liftC = lift

runRenderM :: RenderM a -> BC.Canvas a
runRenderM = flip SS.evalStateStackT def

move :: (Float, Float) -> RenderM ()
move p = do csPos .= p

save :: RenderM ()
save = SS.save >> liftC (BC.save ())

restore :: RenderM ()
restore = liftC (BC.restore ()) >> SS.restore

newPath :: RenderM ()
newPath = liftC $ BC.beginPath ()

closePath :: RenderM ()
closePath = liftC $ BC.closePath ()

arc :: Double -> Double -> Double -> Double -> Double -> RenderM ()
arc a b c d e = liftC $ BC.arc (realToFrac a, realToFrac b, realToFrac c, realToFrac d, realToFrac e,True)

moveTo :: Double -> Double -> RenderM ()
moveTo x y = do
  let x' = realToFrac x
      y' = realToFrac y
  liftC $ BC.moveTo (x', y')
  move (x', y')

relLineTo :: Double -> Double -> RenderM ()
relLineTo x y = do
  p <- use csPos
  let p' = p + (realToFrac x, realToFrac y)
  liftC $ BC.lineTo p'
  move p'

relCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> RenderM ()
relCurveTo ax ay bx by cx cy = do
  p <- use csPos
  let [(ax',ay'),(bx',by'),(cx',cy')] = map ((p +) . (realToFrac *** realToFrac))
                                          [(ax,ay),(bx,by),(cx,cy)]
  liftC $ BC.bezierCurveTo (ax',ay',bx',by',cx',cy')
  move (cx', cy')

-- | Get an accumulated style attribute from the render monad state.
getStyleAttrib :: AttributeClass a => (a -> b) -> RenderM (Maybe b)
getStyleAttrib f = (fmap f . getAttr) <$> use accumStyle

stroke :: RenderM ()
stroke = do

  -- From the HTML5 canvas specification regarding line width:
  --
  --   "On setting, zero, negative, infinite, and NaN values must be
  --   ignored, leaving the value unchanged; other values must change
  --   the current value to the new value.
  --
  -- Hence we must implement a line width of zero by simply not
  -- sending a stroke command.

  -- default value of 1 is arbitary, anything > 0 will do.
  w <- fromMaybe 1 <$> getStyleAttrib (fromOutput . getLineWidth)
  when (w > 0) (liftC $ BC.stroke ())

fill :: RenderM ()
fill = liftC $ BC.fill ()

byteRange :: Double -> Word8
byteRange d = floor (d * 255)

showColorJS :: (Color c) => c -> Text
showColorJS c = T.concat
    [ "rgba("
        , s r, ","
    , s g, ","
    , s b, ","
    , T.pack (show a)
    , ")"
    ]

  where s :: Double -> Text
        s = T.pack . show . byteRange
        (r,g,b,a) = colorToSRGBA c

transform :: Double -> Double -> Double -> Double -> Double -> Double -> RenderM ()
transform ax ay bx by tx ty = liftC $ BC.transform vs
    where 
      vs = (realToFrac ax,realToFrac ay
           ,realToFrac bx,realToFrac by
           ,realToFrac tx,realToFrac ty)

strokeColor :: Texture -> RenderM ()
strokeColor (SC (SomeColor c)) = liftC $ BC.strokeStyle (showColorJS c)

fillColor :: Texture  -> RenderM ()
fillColor (SC (SomeColor c)) = liftC $ BC.fillStyle (showColorJS c)

fromLineCap :: LineCap -> Text
fromLineCap LineCapRound  = T.pack $ show "round"
fromLineCap LineCapSquare = T.pack $ show "square"
fromLineCap _             = T.pack $ show "butt"

fromLineJoin :: LineJoin -> Text
fromLineJoin LineJoinRound = T.pack $ show "round"
fromLineJoin LineJoinBevel = T.pack $ show "bevel"
fromLineJoin _             = T.pack $ show "miter"

-- TODO: update the transform's state for translate, scale, and rotate
-- translate :: Double -> Double -> Render ()
-- translate x y = canvas $ BC.translate (realToFrac x,realToFrac y)

-- scale :: Double -> Double -> Render ()
-- scale x y = canvas $ BC.scale (realToFrac x,realToFrac y)

-- rotate :: Double -> Render ()
-- rotate t = canvas $ BC.rotate (realToFrac t)

-- withStyle :: Render () -> Render () -> Render () -> Render ()
-- withStyle t s r = do
  -- save
  -- r >> t >> s
  -- stroke
  -- fill
  -- restore
