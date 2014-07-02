{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- The Canvas backend.

module Diagrams.Backend.Canvas

  ( Canvas(..) -- rendering token
  , B
  , Options(..) -- for rendering options specific to Canvas
  ) where

import           Control.Lens                 (Lens', (%=), lens, op)
import           Control.Monad.State          (when, State, evalState)

import qualified Data.Foldable as F
import           Data.Maybe                   (catMaybes, isJust, fromJust, fromMaybe)
import qualified Data.Text                    as T
import           Data.Tree                    (Tree(Node))
import           Data.Typeable                (Typeable)

import           Diagrams.Prelude
import           Diagrams.TwoD.Adjust         (adjustDia2D)
import           Diagrams.TwoD.Attributes     (splitTextureFills)
import           Diagrams.TwoD.Path           (Clip (Clip))
import           Diagrams.TwoD.Text
import           Diagrams.TwoD.Types          (R2(..))

import           Diagrams.Core.Compile
import           Diagrams.Core.Types          (Annotation (..))

import qualified Graphics.Blank               as BC
import qualified Graphics.Rendering.Canvas    as C
import           Graphics.Rendering.Canvas    (liftC, getStyleAttrib, accumStyle)

-- | This data declaration is simply used as a token to distinguish 
--   this rendering engine.
data Canvas = Canvas
    deriving Typeable

type B = Canvas

instance Monoid (Render Canvas R2) where
  mempty  = C $ return ()
  (C c1) `mappend` (C c2) = C (c1 >> c2)

instance Backend Canvas R2 where
  data Render  Canvas R2 = C (C.RenderM ())
  type Result  Canvas R2 = BC.Canvas ()
  data Options Canvas R2 = CanvasOptions
          { _canvasSize   :: SizeSpec2D   -- ^ the requested size
          }

  renderRTree :: Canvas -> Options Canvas R2 -> RTree Canvas R2 Annotation 
                        -> Result Canvas R2
  renderRTree _ _ rt = evalState canvasOutput initialCanvasRenderState
    where
      canvasOutput :: State CanvasRenderState (BC.Canvas ())
      canvasOutput = do
        let C r = toRender rt
        return $ C.runRenderM $ r

  adjustDia c opts d = adjustDia2D size c opts (d # reflectY)

runC :: Render Canvas R2 -> C.RenderM ()
runC (C r) = r

toRender :: RTree Canvas R2 Annotation -> Render Canvas R2
toRender = fromRTree
  . Node (RStyle (mempty # recommendFillColor (transparent :: AlphaColour Double)))
  . (:[])
  . splitTextureFills
    where
      fromRTree (Node (RPrim p) _) = render Canvas p
      fromRTree (Node (RStyle sty) rs) = C $ do
        C.save
        canvasStyle sty
        accumStyle %= (<> sty)
        runC $ F.foldMap fromRTree rs
        C.restore
      fromRTree (Node _ rs) = F.foldMap fromRTree rs

data CanvasRenderState = CanvasRenderState

initialCanvasRenderState :: CanvasRenderState
initialCanvasRenderState = CanvasRenderState

getSize :: Options Canvas R2 -> SizeSpec2D
getSize (CanvasOptions {_canvasSize = s}) = s

setSize :: Options Canvas R2 -> SizeSpec2D -> Options Canvas R2
setSize o s = o {_canvasSize = s}

size :: Lens' (Options Canvas R2) SizeSpec2D
size = lens getSize setSize

renderC :: (Renderable a Canvas, V a ~ R2) => a -> C.RenderM ()
renderC a = case (render Canvas a) of C r -> r

canvasStyle :: Style v -> C.RenderM ()
canvasStyle s = sequence_
              . catMaybes $ [ handle clip 
                            , handle lWidth
                            , handle lCap
                            , handle lJoin
                            ]
  where handle :: (AttributeClass a) => (a -> C.RenderM ()) -> Maybe (C.RenderM ())
        handle f = f `fmap` getAttr s
        clip     = mapM_ (\p -> canvasPath p >> C.clip) . op Clip
        lWidth = liftC . BC.lineWidth . realToFrac . fromOutput . getLineWidth
        lCap = liftC . BC.lineCap . C.fromLineCap . getLineCap
        lJoin = liftC .  BC.lineJoin . C.fromLineJoin . getLineJoin

instance Renderable (Segment Closed R2) Canvas where
  render _ (Linear (OffsetClosed (R2 x y))) = C $ C.relLineTo x y
  render _ (Cubic (R2 x1 y1)
                  (R2 x2 y2)
                  (OffsetClosed (R2 x3 y3)))
    = C $ C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail R2) Canvas where
  render _ = withTrail renderLine renderLoop
    where
      renderLine ln = C $ do
        mapM_ renderC (lineSegments ln)
      renderLoop lp = C $ do
        case loopSegments lp of
          (segs, Linear _) -> mapM_ renderC segs
          _ -> mapM_ renderC (lineSegments . cutLoop $ lp)
        C.closePath

instance Renderable (Path R2) Canvas where
  render _ p = C $ do
    canvasPath p
    f <- getStyleAttrib getFillTexture
    s <- getStyleAttrib getLineTexture
    o <- fromMaybe 1 <$> getStyleAttrib getOpacity
    C.save
    when (isJust f) (C.fillTexture (fromJust f) o >> C.fill)
    C.strokeTexture (fromMaybe (SC (SomeColor (black :: Colour Double))) s) o
    C.stroke
    C.restore

-- Add a path to the Canvas context, without stroking or filling it.
canvasPath :: Path R2 -> C.RenderM ()
canvasPath (Path trs) = do
    C.newPath
    F.mapM_ renderTrail trs
  where
    renderTrail (viewLoc -> (unp2 -> p, tr)) = do
      uncurry C.moveTo p
      renderC tr

instance Renderable Text Canvas where
  render _ (Text tt tn al str) = C $ do
    isLocal <- fromMaybe True <$> getStyleAttrib getFontSizeIsLocal
    tf      <- fromMaybe "Calibri" <$> getStyleAttrib getFont
    fs      <- fromMaybe 12 <$> getStyleAttrib (fromOutput . getFontSize)
    slant   <- fromMaybe FontSlantNormal <$> getStyleAttrib getFontSlant
    fw      <- fromMaybe FontWeightNormal <$> getStyleAttrib getFontWeight
    tx      <- fromMaybe (SC (SomeColor (black :: Colour Double)))
               <$> getStyleAttrib getFillTexture
    o       <- fromMaybe 1 <$> getStyleAttrib getOpacity
    let fnt = C.showFontJS fw slant fs tf
        tr  = if isLocal then tt else tn
        vAlign = case al of
                   BaselineText -> T.pack "alphabetic"
                   BoxAlignedText _ h -> case h of
                     h' | h' <= 0.25 -> T.pack "bottom"
                     h' | h' >= 0.75 -> T.pack "top"
                     _ -> T.pack "middle"
        hAlign = case al of
                   BaselineText -> T.pack "start"
                   BoxAlignedText w _ -> case w of
                     w' | w' <= 0.25 -> T.pack "start"
                     w' | w' >= 0.75 -> T.pack "end"
                     _ -> T.pack "center"
    C.save
    C.liftC $ BC.textBaseline vAlign
    C.liftC $ BC.textAlign hAlign
    C.liftC $ BC.font fnt
    C.strokeTexture tx o
    C.canvasTransform (tr <> reflectionY)
    C.liftC $ BC.fillText (T.pack str, 0, 0)
    C.restore

-- instance Renderable (DImage External) Canvas where
  -- render _ (DImage path w h tr) = C $ do
    -- let ImageRef file = path
    -- C.save
    -- C.canvasTransform (tr <> reflectionY)
    -- img <- C.liftC $ BC.newImage (T.pack file)
    -- C.liftC $ BC.drawImage (img, [0, 0, fromIntegral w, fromIntegral h])
    -- C.restore
