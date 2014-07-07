{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE CPP                  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Canvas.CmdLine
-- Copyright   :  (c) 2011-2014 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the Canvas backend.
--
--
-- * 'defaultMain' creates an executable which can render a single
--   diagram at various options.
--
-- * 'multiMain' is like 'defaultMain' but allows for a list of
--   diagrams from which the user can choose one to render.
--
-- * 'mainWith' is a generic form that does all of the above but with
--   a slightly scarier type.  See "Diagrams.Backend.CmdLine".  This
--   form can also take a function type that has a suitable final result
--   (any of arguments to the above types) and 'Parseable' arguments.
--
-- If you want to generate diagrams programmatically---/i.e./ if you
-- want to do anything more complex than what the below functions
-- provide---you have several options.
--
-- * Use a function with 'mainWith'.  This may require making
--   'Parseable' instances for custom argument types.
--
-- * Make a new 'Mainable' instance.  This may require a newtype
--   wrapper on your diagram type to avoid the existing instances.
--   This gives you more control over argument parsing, intervening
--   steps, and diagram creation.
--
-- * Build option records and pass them along with a diagram to 'mainRender'
--   from "Diagrams.Backend.CmdLine".
--
-- For a tutorial on command-line diagram creation see
-- <http://projects.haskell.org/diagrams/doc/cmdline.html>.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Canvas.CmdLine
       ( 
        -- * General form of @main@
        --  $mainWith
        mainWith

        -- * Supported froms of @main@
       , defaultMain
       , multiMain
       , Canvas
       , B
       ) where

import Diagrams.Prelude                hiding (width, height, option, (<>), value)
import Diagrams.Backend.CmdLine        hiding (width, height)
import Diagrams.Backend.Canvas
import qualified Graphics.Blank        as BC

import Data.Data
import Control.Lens                    (makeLenses, (^.))
import Options.Applicative

data DiaOpts = DiaOpts 
  { _width  :: Maybe Int -- ^ Final output width of diagram.
  , _height :: Maybe Int -- ^ Final height of diagram.
  , _port   :: Int       -- ^ Port on which to start web server.
  } deriving (Show, Data, Typeable)

makeLenses ''DiaOpts

diaOpts :: Parser DiaOpts
diaOpts = DiaOpts
  <$> (optional . option)
      (long "width" <> short 'w'
    <> metavar "WIDTH"
    <> help "Desired WIDTH of the output image")
  <*> (optional . option)
      (long "height" <> short 'h'
    <> metavar "HEIGHT"
    <> help "Desired HEIGHT of the output image")
  <*> option
      (long "port" <> short 'p' 
    <> value 3000
    <> metavar "PORT"
    <> help "Port on which to satrt the web server (default 3000)")

instance Parseable DiaOpts where
  parser = diaOpts
  
defaultMain :: Diagram Canvas R2 -> IO ()
defaultMain = mainWith
    
instance Mainable (Diagram Canvas R2) where
  type MainOpts (Diagram Canvas R2) = DiaOpts
  
  mainRender opts d = canvasRender opts d

canvasRender :: DiaOpts -> Diagram Canvas R2 -> IO ()
canvasRender opts d = BC.blankCanvas (fromIntegral (opts^.port)) (canvasDia opts d)

canvasDia :: DiaOpts -> Diagram Canvas R2 -> BC.DeviceContext -> IO ()
canvasDia opts d context = do
  BC.send context $
    renderDia 
      Canvas 
      (CanvasOptions 
        (mkSizeSpec 
          (fromIntegral <$> opts^.width) 
          (fromIntegral <$> opts^.height))) 
    d

multiMain :: [(String, Diagram Canvas R2)] -> IO ()
multiMain = mainWith

instance Mainable [(String, Diagram Canvas R2)] where
  type MainOpts [(String, Diagram Canvas R2)] = 
    (MainOpts (Diagram Canvas R2), DiagramMultiOpts)

  mainRender = defaultMultiMainRender
