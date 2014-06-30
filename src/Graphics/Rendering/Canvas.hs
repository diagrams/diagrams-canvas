{-# LANGUAGE GeneralizedNewtypeDeriving
           , RecordWildCards
           , OverloadedStrings 
           , TemplateHaskell #-}

module Graphics.Rendering.Canvas
  ( RenderM()
  , liftC
  , getStyleAttrib
  , runRenderM
  , accumStyle
  , newPath
  , moveTo
  , relLineTo
  , relCurveTo
  , closePath
  , stroke
  , fill
  , clip
  , canvasTransform
  , save
  , restore
  , strokeTexture
  , fillTexture
  , showColorJS
  , showFontJS
  , fromLineCap
  , fromLineJoin
  ) where

import           Control.Applicative      ((<$>))
import           Control.Arrow            ((***))
import           Control.Lens             (makeLenses, (.=), use, (^.))
import           Control.Monad.State
import qualified Control.Monad.StateStack as SS

import           Data.Default.Class       (Default(..))
import           Data.Maybe (fromMaybe)
import           Data.NumInstances        ()
import qualified Data.Text                as T
import           Data.Text                (Text)
import           Data.Word                (Word8)

import           Diagrams.Prelude         (Monoid(mempty))
import           Diagrams.Attributes      (Color(..),LineCap(..),LineJoin(..), 
                                          SomeColor(..), colorToSRGBA)
import           Diagrams.Core.Style      (Style, AttributeClass, getAttr)
import           Diagrams.Core.Transform
import           Diagrams.Core.Types      (fromOutput)
import           Diagrams.TwoD.Attributes hiding (fillTexture)
import           Diagrams.TwoD.Text       hiding (Text)
import           Diagrams.TwoD.Types      (R2(..), unp2, T2)
import qualified Graphics.Blank           as BC
import qualified Graphics.Blank.Style     as S

data CanvasState = CanvasState { _accumStyle :: Style R2
                               , _csPos :: (Float, Float) }

makeLenses ''CanvasState

instance Default CanvasState where
  def = CanvasState { _accumStyle = mempty
                    , _csPos = (0,0) }

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

clip :: RenderM ()
clip = liftC $ BC.clip ()

byteRange :: Double -> Word8
byteRange d = floor (d * 255)

data TextureUse = Fill | Line

texture :: TextureUse -> Texture -> RenderM()
texture u (SC (SomeColor c)) = case u of
  Fill -> liftC . S.fillStyle   $ s
  Line -> liftC . S.strokeStyle $ s
  where s = showColorJS c

texture u (LG g) = liftC $ do
  grd <- BC.createLinearGradient (x0, y0, x1, y1)
  mapM_ (flip BC.addColorStop $ grd) stops
  case u of
    Fill -> S.fillStyle grd
    Line -> S.strokeStyle grd
  where
    (x0', y0') = unp2 $ transform (g^.lGradTrans) (g^.lGradStart)
    (x1', y1') = unp2 $ transform (g^.lGradTrans) (g^.lGradEnd)
    (x0, y0, x1, y1) = ( realToFrac x0', realToFrac y0'
                       , realToFrac x1', realToFrac y1')
    stops = map (\s -> ( realToFrac (s^.stopFraction)
                       , showColorJS (s^.stopColor))) (g^.lGradStops)

texture u (RG g) = liftC $ do
  grd <- BC.createRadialGradient (x0, y0, r0, x1, y1, r1)
  mapM_ (flip BC.addColorStop $ grd) stops
  case u of
    Fill -> S.fillStyle grd
    Line -> S.strokeStyle grd
  where
    (r0, r1) = (realToFrac (g^.rGradRadius0), realToFrac (g^.rGradRadius1))
    (x0', y0') = unp2 $ transform (g^.rGradTrans) (g^.rGradCenter0)
    (x1', y1') = unp2 $ transform (g^.rGradTrans) (g^.rGradCenter1)
    (x0, y0, x1, y1) = ( realToFrac x0', realToFrac y0'
                       , realToFrac x1', realToFrac y1')
    stops = map (\s -> ( realToFrac (s^.stopFraction)
                       , showColorJS (s^.stopColor))) (g^.rGradStops)

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

canvasTransform :: T2 -> RenderM ()
canvasTransform tr = liftC $ BC.setTransform vs
    where 
      [[ax, ay], [bx, by], [tx, ty]] = matrixHomRep tr
      vs = (realToFrac ax,realToFrac ay
           ,realToFrac bx,realToFrac by
           ,realToFrac tx,realToFrac ty)

strokeTexture :: Texture -> RenderM ()
strokeTexture = texture Line

fillTexture :: Texture  -> RenderM ()
fillTexture = texture Fill

fromLineCap :: LineCap -> Text
fromLineCap LineCapRound  = "round"
fromLineCap LineCapSquare = "square"
fromLineCap _             = "butt"

fromLineJoin :: LineJoin -> Text
fromLineJoin LineJoinRound = "round"
fromLineJoin LineJoinBevel = "bevel"
fromLineJoin _             = "miter"

showFontJS :: FontWeight -> FontSlant -> Double -> String -> Text
showFontJS wgt slant size fnt = T.concat [a, " ", b, " ", c, " ", d]
  where
    a = case wgt of
          FontWeightNormal -> ""
          FontWeightBold   -> "bold"
    b = case slant of
          FontSlantNormal  -> ""
          FontSlantItalic  -> "italic"
          FontSlantOblique -> "oblique"
    c = T.concat [T.pack $ show size, "pt"]
    d = T.pack fnt
