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
import Data.DList(DList,toList,fromList,append)
import Data.Word(Word8)
import Data.Monoid
import Data.NumInstances
import System.IO (openFile, hPutStr, IOMode(..), hClose)

type RGBA = (Double, Double, Double, Double)

data DrawState = DS
                 { dsPos :: R2
                 , dsFill :: RGBA
                 , dsStroke :: RGBA
                 , dsCap :: LineCap
                 , dsJoin :: LineJoin
                 , dsWidth :: Double
                 , dsTransform :: [Double]
                 } deriving (Eq)

emptyDS :: DrawState
emptyDS = DS 0 (0,0,0,1) 0 LineCapButt LineJoinMiter 0 []

data RenderState = RS
                   { drawState :: DrawState
                   , saved :: [DrawState]
                   , result :: DList String
                   }

emptyRS :: RenderState
emptyRS = RS emptyDS [] mempty

newtype Render m = Render { runRender :: StateT RenderState IO m }
  deriving (Functor, Monad, MonadState RenderState)

data Surface = Surface { header :: String, footer :: String, width :: Int, height :: Int, fileName :: String }

write :: DList String -> Render ()
write s = modify $ \rs@(RS{..}) -> rs { result = result `append` s }

move :: R2 -> Render ()
move p = modify $ \rs@(RS{..}) -> rs { drawState = drawState { dsPos = p } }

setDS :: DrawState -> Render ()
setDS d = modify $ (\rs -> rs { drawState = d })

saveRS :: Render ()
saveRS = modify $ \rs@(RS{..}) -> rs { saved = drawState : saved }

restoreRS = modify go
  where
    go rs@(RS{saved = d:ds, ..}) = rs { drawState = d, saved = ds }
    go rs = rs

at :: Render R2
at = (dsPos . drawState) <$> get

renderWith :: MonadIO m => Surface -> Render a -> m a
renderWith s r = liftIO $ do
  (v,rs) <- runStateT (runRender r) emptyRS
  h <- openFile (fileName s) WriteMode
  hPutStr h (header s)
  mapM_ (hPutStr h) (toList (result rs))
  hPutStr h (footer s)
  hClose h
  return v

withJSSurface :: String -> Int -> Int -> (Surface -> IO a) -> IO a
withJSSurface file w h f = f s
  where s = Surface jsHeader jsFooter w h file

withHTMLSurface :: String -> Int -> Int -> (Surface -> IO a) -> IO a
withHTMLSurface file w h f = f s
  where s = Surface htmlHeader (htmlFooter w h) w h file

renderJS :: String -> Render ()
renderJS s = write $ fromList [jsPrefix, s, ";\n"]

mkJSCall :: Show a => String -> [a] -> Render()
mkJSCall n vs = renderJS . concat $ [n, "("] ++ intersperse "," (map show vs) ++ [")"]

newPath :: Render ()
newPath = renderJS "beginPath()"

closePath :: Render ()
closePath = renderJS "closePath()"

arc :: Double -> Double -> Double -> Double -> Double -> Render ()
arc a b c d e = mkJSCall "arcTo" [a,b,c,d,e]

moveTo :: Double -> Double -> Render ()
moveTo x y = do
  mkJSCall "moveTo" [x,y]
  move (x,y)

lineTo :: Double -> Double -> Render ()
lineTo x y = do
  p <- at
  let p'@(x',y') = p + (x,y)
  mkJSCall "lineTo" [x',y']
  move p'

curveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
curveTo ax ay bx by cx cy = do
--  lineTo cx cy
  p <- at
  let ps = map (p +) [(ax,ay),(bx,by),(cx,cy)]
  mkJSCall "bezierCurveTo" (concatMap (\(a,b) -> [a,b]) ps)
  move (last ps)

stroke :: Render ()
stroke = renderJS "stroke()"

fill :: Render ()
fill = renderJS "fill()"

save :: Render ()
save = saveRS >> renderJS "save()"

restore :: Render ()
restore = restoreRS >> renderJS "restore()"

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
                              (mkJSCall "transform" vs)
    where vs = [ax,ay,bx,by,tx,ty]

strokeColor :: (Color c) => c -> Render ()
strokeColor c = setDSWhen
                (\ds -> ds { dsStroke = colorToRGBA c})
                (renderJS $ "strokeStyle = " ++ showColorJS c)

fillColor :: (Color c) => c -> Render ()
fillColor c = setDSWhen
              (\ds -> ds { dsFill = colorToRGBA c })
              (renderJS $ "fillStyle = " ++ showColorJS c)

lineWidth :: Double -> Render ()
lineWidth w = setDSWhen
              (\ds -> ds { dsWidth = w })
              (renderJS $ "lineWidth = " ++ show w)

lineCap :: LineCap -> Render ()
lineCap lc = setDSWhen
             (\ds -> ds { dsCap = lc })
             (renderJS $ "lineCap = " ++ fromLineCap lc)

lineJoin :: LineJoin -> Render ()
lineJoin lj = setDSWhen
              (\ds -> ds { dsJoin = lj })
              (renderJS $ "lineJoin = " ++ fromLineJoin lj)

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
translate x y = mkJSCall "translate" [x,y]

scale :: Double -> Double -> Render ()
scale x y = mkJSCall "scale" [x,y]

rotate :: Double -> Render ()
rotate t = mkJSCall "rotate" [t]

withStyle :: Render () -> Render () -> Render () -> Render ()
withStyle t s r = do
  save
  r >> t >> s
  stroke
  fill
  restore

jsHeader = "    function renderDiagram(c) {\n"
        ++ jsPrefix ++ "fillStyle = \"rgba(0,0,0,0.0)\";\n"
        ++ jsPrefix ++ "strokeStyle = \"rgba(0,0,0,1.0)\";\n"
        ++ jsPrefix ++ "miterLimit = 10;\n"
jsFooter = "    }\n"

jsPrefix = "      c."

htmlHeader = concat
          [ "<!DOCTYPE HTML>\n\
            \<html>\n\
            \  <head>\n\
            \  <script type=\"application/javascript\">\n\
            \    function draw() {  \n\
            \      var canvas = document.getElementById(\"canvas\");\n\
            \      if (canvas.getContext) {\n\
            \        var context = canvas.getContext(\"2d\");\n\
            \        renderDiagram(context);\n\
            \      }\n\
            \    }\n\n"
          , jsHeader
          ]
htmlFooter w h = concat
          [ jsFooter
          , " </script>\n\
            \ </head>\n\
            \ <body onload=\"draw();\">\n\
            \   <canvas id=\"canvas\" width=\"", show w, "\" height=\"", show h, "\"></canvas>\n\
            \ </body>\n\
            \</html>"
          ]
