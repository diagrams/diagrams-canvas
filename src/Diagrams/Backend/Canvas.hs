{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Canvas
-- Copyright   :  (c) 2010 - 2018 diagrams-canvas team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A full-featured rendering backend for diagrams using Canvas.
-- Implemented using the blank-canvas platform.
--
-- To invoke the Canvas backend, you have three options.
--
-- * You can use the "Diagrams.Backend.Canvas.CmdLine" module to create
--   standalone executables which will display the diagram in a browser
--   using a web service.
--
-- * You can use the 'renderCanvas' function provided by this module,
--   which gives you more programmatic control over when and
--   how images are displayed (making it east to, for example, write a
--   single program that displays multiple images, or one that diaplays
--   images dynamically based on user input, and so on).
--
-- * For the most flexiblity you can invoke the 'renderDia' method from
--   'Diagrams.Core.Types.Backend' instance for @Canvas@. In particular,
--   'Diagrams.Core.Types.renderDia' has the generic type
--
-- > renderDia :: b -> Options b v -> QDiagram b v m -> Result b v
--
-- (omitting a few type class contraints). @b@ represents the
-- backend type, @v@ the vector space, and @m@ the type of monoidal
-- query annotations on the diagram. 'Options' and 'Result' are
-- associated data and type families, respectively, which yield the
-- type of option records and rendering results specific to any
-- particular backend. For @b ~ Canvas@ and @v ~ R2@, we have
--
-- > data Options Canvas V2 Double = CanvasOptions
-- >  { _size :: SizeSpec V2 -- ^^ The requested size
-- >  }
--
-- @
-- data family Render Canvas V2 Double = C (RenderM ())
-- @
--
-- @
-- type family Result Canvas V2 Double = Canvas ()
-- @
--
-- So the type of 'renderDia' resolves to
--
-- @
-- renderDia :: Canvas -> Options Canvas V2 Double -> QDiagram Canvas V2 Double m ->
-- Canvas()
-- @
--
-- which you could call like @renderDia Canvas (CanvasOptions (width 250))
-- myDiagram@
--
------------------------------------------------------------------------------

module Diagrams.Backend.Canvas

  ( Canvas(..) -- rendering token
  , Options(..) -- for rendering options specific to Canvas

  , renderCanvas
  , canvasSize

  ) where

import           Control.Lens             hiding (transform, ( # ))
import           Control.Monad.State
import           Control.Monad.Trans      (lift)

import qualified Options.Applicative as OP

import           Data.Default.Class
import qualified Data.Foldable            as F
import           Data.Maybe               (fromMaybe)
import           Data.NumInstances        ()
import qualified Data.Text                as T
import           Data.Typeable            (Typeable)
import           Data.Word                (Word8)

import           Diagrams.Attributes
import           Diagrams.Backend.Compile
import           Diagrams.Prelude         hiding (clip, fillTexture, moveTo,
                                           stroke)
import           Diagrams.TwoD.Text
import           Diagrams.Types

import           Data.Sequence            (Seq)
import qualified Graphics.Blank           as BC
import qualified Graphics.Blank.Style     as S

-- | This data declaration is simply used as a token to distinguish
--   this rendering engine.
data Canvas = Canvas
    deriving (Eq, Ord, Read, Show, Typeable)

type instance V Canvas = V2
type instance N Canvas = Double

default2DAttrs :: Diagram V2 -> Diagram V2
default2DAttrs = lineWidth medium

instance Backend Canvas where
  -- data Render  Canvas V2 Double = C (RenderM ())
  type Result  Canvas = BC.Canvas ()
  data Options Canvas = CanvasOptions
         { _canvasSize   :: SizeSpec V2 Int
         }

  backendInfo _ = canvasInfo

  renderDiaT opts dia = (sz, t2 <> reflectionY, drawing) where
    (sz, t2, dia') =
      adjustSize2D (_canvasSize opts) (default2DAttrs dia # reflectY)
    drawing        = render t2 dia'

instance RenderOutcome Canvas (Diagram V2) where
  type MainOpts Canvas (Diagram V2) = (SizeSpec V2 Int, Int)

  resultParser _ _ = (,) <$> sizeParser <*> portParser
  renderOutcome _ (sz, port) = renderCanvas port sz

portParser :: OP.Parser Int
portParser = OP.option OP.auto $
  mconcat
    [ OP.long "port"
    , OP.value 3000
    , OP.metavar "PORT"
    , OP.help "Port on which to start the web server"
    , OP.showDefault
    ]


canvasSize :: Lens' (Options Canvas) (SizeSpec V2 Int)
canvasSize = lens _canvasSize (\o s -> o {_canvasSize = s})

render :: T2 Double -> Diagram V2 -> BC.Canvas ()
render = foldDiaA renderPrim renderAnnot where
  renderPrim t2 attrs prim =
    case renderPrimitive t2 attrs prim of
      Just r  -> r
      Nothing -> error "Unknown primitive"

renderPrimitive
  :: T2 Double -> Attributes -> Prim V2 Double -> Maybe (BC.Canvas ())
renderPrimitive t2 attrs = \case
  Path_ path              -> Just $ drawPath attrs (transform t2 path)
  Text_ t                 -> Just $ drawText t2 attrs t
  ExternalImage_ x y path -> Just $ externalImage t2 attrs x y path
  Prim _                  -> Nothing

renderAnnot :: Annotation V2 Double -> BC.Canvas () -> BC.Canvas ()
renderAnnot = \case
  -- GroupOpacity_ x -> R.withGroupOpacity (round $ 255 * x)
  Clip_ c         -> clip c
  _               -> id

clip :: Seq (Path V2 Double) -> BC.Canvas () -> BC.Canvas ()
clip ps r = do
  BC.save ()
  F.for_ ps $ \path -> do
    BC.beginPath ()
    mapMOf_ each drawTrail path
    BC.clip ()
  r
  BC.restore ()

------------------------------------------------------------------------

byteRange :: Double -> Word8
byteRange d = floor (d * 255)

texture :: (forall a. S.Style a => a -> BC.Canvas ()) -> Texture -> Double -> BC.Canvas ()
texture styleFn (SC (SomeColor c))  o = styleFn s
  where s = showColorJS c o

texture styleFn (LG g) _ = do
  grd <- BC.createLinearGradient (x0, y0, x1, y1)
  mapM_ (flip BC.addColorStop $ grd) stops
  styleFn grd
  where
    (x0, y0) = unp2 $ transform (g^.gradientTransform) (g^.gradientStart)
    (x1, y1) = unp2 $ transform (g^.gradientTransform) (g^.gradientEnd)
    stops = map (\s -> ( s^.stopFraction , showColorJS (s^.stopColor) 1)) (g^.gradientStops)

texture styleFn (RG g) _ = do
  grd <- BC.createRadialGradient (x0, y0, r0, x1, y1, r1)
  mapM_ (flip BC.addColorStop $ grd) stops
  styleFn grd
  where
    (r0, r1) = (s * g^.gradientRadius0, s * g^.gradientRadius1)
    (x0, y0) = unp2 $ transform (g^.gradientTransform) (g^.gradientCenter0)
    (x1, y1) = unp2 $ transform (g^.gradientTransform) (g^.gradientCenter1)
    stops = map (\st -> ( st^.stopFraction , showColorJS (st^.stopColor) 1)) (g^.gradientStops)
    s = avgScale $ g^.gradientTransform

showColorJS :: (Color c) => c -> Double  -> T.Text
showColorJS c o = T.concat
    [ "rgba("
    , s (realToFrac r), ","
    , s (realToFrac g), ","
    , s (realToFrac b), ","
    , T.pack (show $ (realToFrac a) * o)
    , ")"
    ]
  where s :: Double -> T.Text
        s = T.pack . show . byteRange
        (r,g,b,a) = colorToSRGBA . toAlphaColour $  c

canvasTransform :: T2 Double -> BC.Canvas ()
canvasTransform tr = BC.transform vs
    where
      [[ax, ay], [bx, by], [tx, ty]] = matrixHomRep tr
      vs = (realToFrac ax,realToFrac ay
           ,realToFrac bx,realToFrac by
           ,realToFrac tx,realToFrac ty)

strokeTexture :: Texture -> Double -> BC.Canvas ()
strokeTexture = texture S.strokeStyle

fillTexture :: Texture -> Double -> BC.Canvas ()
fillTexture = texture S.fillStyle

fromLineCap :: LineCap -> BC.LineEndCap
fromLineCap LineCapRound  = BC.RoundCap
fromLineCap LineCapSquare = BC.SquareCap
fromLineCap _             = BC.ButtCap

fromLineJoin :: LineJoin -> BC.LineJoinCorner
fromLineJoin LineJoinRound = BC.RoundCorner
fromLineJoin LineJoinBevel = BC.BevelCorner
fromLineJoin _             = BC.MiterCorner

showFontJS :: FontWeight -> FontSlant -> Double -> String -> T.Text
showFontJS wgt slant sz fnt = T.concat [a, " ", b, " ", c, " ", d]
  where
    a = case wgt of
          FontWeightNormal -> ""
          FontWeightBold   -> "bold"
          _                -> ""
    b = case slant of
          FontSlantNormal  -> ""
          FontSlantItalic  -> "italic"
          FontSlantOblique -> "oblique"
    c = T.concat [T.pack $ show sz, "pt"]
    d = T.pack fnt

canvasStyle :: Attributes -> BC.Canvas ()
canvasStyle attrs = do

  let attr :: (Default a, Typeable a) => Getting r a r -> r
      attr g   = fromMaybe (def^.g) $ getAttr g attrs
  let
      opa      = attr _Opacity
      fTexture = attr _FillTexture
      lTexture = attr _LineTexture
      -- fRule    = fromFillRule (attr _FillRule)

      lWidth   = realToFrac @Double $ attr _LineWidth
      -- lCap     = fromLineCap (attr _LineCap)
      -- lDash    = fmap fromDashing (getAttr _Dashing s)

  texture S.fillStyle fTexture opa
  -- BC.fill ()
  texture S.strokeStyle lTexture opa
  BC.lineWidth lWidth
  BC.lineCap (fromLineCap $ attr _LineCap)
  BC.lineJoin (fromLineJoin $ attr _LineJoin)

  -- let w = fromMaybe 0 $ getAttr _LineWidth attrs

  return ()

drawSegment :: Segment V2 Double -> StateT (P2 Double) BC.Canvas ()
drawSegment = \case
  Linear v       -> relLineTo v
  Cubic v1 v2 v3 -> relCurveTo v1 v2 v3

relLineTo :: V2 Double -> StateT (P2 Double) BC.Canvas ()
relLineTo v = do
  p <- get
  let p' = p .+^ v
  lift $ BC.lineTo (unp2 p')
  put p'

relCurveTo :: V2 Double -> V2 Double -> V2 Double -> StateT (P2 Double) BC.Canvas ()
relCurveTo v1 v2 v3 = do
  p <- get
  let P2 x1 y1 = p .+^ v1
  let P2 x2 y2 = p .+^ v2
  let P2 x3 y3 = p .+^ v3
  lift $ BC.bezierCurveTo (x1,y1,x2,y2,x3,y3)
  put (P2 x3 y3)

drawTrail :: Located (Trail V2 Double) -> BC.Canvas ()
drawTrail (Loc p t) = do
  BC.moveTo (unp2 p)
  evalStateT (mapMOf_ segments drawSegment t) p
  when (has _Loop t) $ BC.closePath ()

drawPath :: Attributes -> Path V2 Double -> BC.Canvas ()
drawPath attrs path = do
  BC.save()
  BC.beginPath ()
  mapMOf_ each drawTrail path
  canvasStyle attrs
  BC.fill ()
  BC.stroke ()
  BC.restore()

drawText :: T2 Double -> Attributes -> Text Double -> BC.Canvas ()
drawText t2 attrs (Text txtAlign str) = do
  let attr :: (Default a, Typeable a) => Getting r a r -> r
      attr g   = fromMaybe (def^.g) $ getAttr g attrs
  let
      tf    = fromMaybe "Calibri" $ getAttr _Font attrs
      fSize = attr _FontSize
      slant = attr _FontSlant
      fw    = attr _FontWeight

      fnt = showFontJS fw slant fSize tf
      vAlign = case txtAlign of
                 BaselineText -> BC.AlphabeticBaseline
                 BoxAlignedText _ h -> case h of
                   h' | h' <= 0.25 -> BC.BottomBaseline
                   h' | h' >= 0.75 -> BC.TopBaseline
                   _ -> BC.MiddleBaseline
      hAlign = case txtAlign of
                 BaselineText -> BC.StartAnchor
                 BoxAlignedText w _ -> case w of
                   w' | w' <= 0.25 -> BC.StartAnchor
                   w' | w' >= 0.75 -> BC.EndAnchor
                   _ -> BC.CenterAnchor
  BC.save()
  BC.textBaseline vAlign
  BC.textAlign hAlign
  BC.font fnt
  canvasStyle attrs
  canvasTransform (t2 <> reflectionY)
  BC.fillText (T.pack str, 0, 0)
  BC.restore()

externalImage :: T2 Double -> Attributes -> Int -> Int -> FilePath -> BC.Canvas ()
externalImage t2 attrs w h path = do
  BC.save()
  canvasTransform (t2 <> reflectionY)
  img <- BC.newImage (T.pack path)
  BC.drawImage
    (img, [fromIntegral (-w) / 2, fromIntegral (-h) / 2, fromIntegral w, fromIntegral h])
  BC.restore()

renderCanvas :: Int -> SizeSpec V2 Int -> Diagram V2 -> IO ()
renderCanvas port sz d =
  BC.blankCanvas (fromIntegral port) . flip BC.send $ img
    where
      img = view _3 $ renderDiaT (CanvasOptions sz) d

