{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , DeriveDataTypeable
  #-}
{-|
  The Canvas backend.
-}
module Diagrams.Backend.Canvas

  ( Canvas(..) -- rendering token

  , Options(..) -- for rendering options specific to Canvas
  , OutputFormat(..) -- output format options
  ) where

import qualified Graphics.Rendering.Canvas as C

import Diagrams.Prelude

import Graphics.Rendering.Diagrams.Transform

import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Adjust (adjustDia2D, adjustSize)

import Control.Monad (when)
import Data.Maybe (catMaybes)

import Data.VectorSpace

import Data.Monoid
import qualified Data.Foldable as F
import Data.Typeable

-- | This data declaration is simply used as a token to distinguish this rendering engine.
data Canvas = Canvas
    deriving Typeable

-- | Canvas is able to output to several file formats, which each have their own associated properties that affect the output.
data OutputFormat = JS | HTML

instance Monoid (Render Canvas R2) where
  mempty  = C $ return ()
  (C r1) `mappend` (C r2) = C (r1 >> r2)


instance Backend Canvas R2 where
  data Render  Canvas R2 = C (C.Render ())
  type Result  Canvas R2 = IO ()
  data Options Canvas R2 = CanvasOptions
          { fileName     :: String       -- ^ the name of the file you want generated
          , canvasSize   :: SizeSpec2D   -- ^ the requested size
          , outputFormat :: OutputFormat -- ^ the output format and associated options
          }

  withStyle _ s t (C r) = C $ do
    C.withStyle (canvasTransf t) (canvasStyle s) r

  doRender _ (CanvasOptions file size out) (C r) =
    let surfaceF surface = C.renderWith surface r
        -- Everything except Dims is arbitrary. The backend
        -- should have first run 'adjustDia' to update the
        -- final size of the diagram with explicit dimensions,
        -- so normally we would only expect to get Dims anyway.
        (w,h) = case size of
                  Width w'   -> (w',w')
                  Height h'  -> (h',h')
                  Dims w' h' -> (w',h')
                  Absolute   -> (100,100)

    in  case out of
          JS   -> C.withJSSurface file (round w) (round h) surfaceF
          HTML -> C.withHTMLSurface file (round w) (round h) surfaceF

  adjustDia c opts d = adjustDia2D canvasSize setCanvasSize c opts (reflectY d)
    where setCanvasSize sz o = o { canvasSize = sz }

renderC :: (Renderable a Canvas, V a ~ R2) => a -> C.Render ()
renderC a = case (render Canvas a) of C r -> r

canvasStyle :: Style v -> C.Render ()
canvasStyle s = foldr (>>) (return ())
              . catMaybes $ [ handle fColor
                            , handle lColor
                            , handle lWidth
                            , handle lJoin
                            , handle lCap
                            ]
  where handle :: (AttributeClass a) => (a -> C.Render ()) -> Maybe (C.Render ())
        handle f = f `fmap` getAttr s
        lColor = C.strokeColor . getLineColor
        fColor = C.fillColor . getFillColor
        lWidth = C.lineWidth . getLineWidth
        lCap = C.lineCap . getLineCap
        lJoin = C.lineJoin . getLineJoin

canvasTransf :: Transformation R2 -> C.Render ()
canvasTransf t = C.transform a1 a2 b1 b2 c1 c2
  where (a1,a2) = apply t (1,0)
        (b1,b2) = apply t (0,1)
        (c1,c2) = transl t

instance Renderable (Segment R2) Canvas where
  render _ (Linear v) = C $ uncurry C.lineTo v
  render _ (Cubic (x1,y1) (x2,y2) (x3,y3)) = C $ C.curveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail R2) Canvas where
  render _ (Trail segs c) = C $ do
    mapM_ renderC segs
    when c $ C.closePath

instance Renderable (Path R2) Canvas where
  render _ (Path trs) = C $ C.newPath >> F.mapM_ renderTrail trs
    where renderTrail (P p, tr) = do
            uncurry C.moveTo p
            renderC tr

absoluteTrail :: R2 -> Trail R2 -> Trail R2
absoluteTrail v (Trail segs c) = Trail (absolute v segs) c

absolute :: R2 -> [Segment R2] -> [Segment R2]
absolute _ [] = []
absolute v (s:ss) = s' : absolute v' ss
  where (v',s') = addV s
        addV (Linear a) = (\p -> (p, Linear p)) (a ^+^ v)
        addV (Cubic a b c) = (c ^+^ v, Cubic (a ^+^ v) (b ^+^ v) (c ^+^ v))
