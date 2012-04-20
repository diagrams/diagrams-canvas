{-# LANGUAGE DeriveDataTypeable, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Canvas.CmdLine
-- Copyright   :  (c) 2011 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the Canvas backend.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Canvas.CmdLine
       ( defaultMain
       , multiMain

       , Canvas
       ) where

import System.Console.CmdArgs.Implicit hiding (args)
import System.Environment (getProgName)

import Diagrams.Prelude hiding (width, height)
import Diagrams.Backend.Canvas
import qualified Graphics.Blank as BC


data DiagramOpts = DiagramOpts
                   { width     :: Maybe Int
                   , height    :: Maybe Int
                   , port      :: Int
                   , selection :: Maybe String
                   }
  deriving (Show, Data, Typeable)

diagramOpts :: String -> Bool -> DiagramOpts
diagramOpts prog sel = DiagramOpts
  { width =  def
             &= typ "INT"
             &= help "Desired width of the output image"

  , height = def
             &= typ "INT"
             &= help "Desired height of the output image"

  -- , output = def
  --          &= typFile
  --          &= help "Output file"

  , port   = 3000
           &= typ "PORT"  
           &= help "Port on which to start the web server (default 3000)"
           
  , selection = def
              &= help "Name of the diagram to render"
              &= (if sel then typ "NAME" else ignore)
  }
  &= summary "Command-line diagram generation."
  &= program prog

defaultMain :: Diagram Canvas R2 -> IO ()
defaultMain d = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog False)
  canvasRender opts d

canvasRender :: DiagramOpts -> Diagram Canvas R2 -> IO ()
canvasRender opts d = BC.blankCanvas (port opts) (canvasDia opts d)

canvasDia :: DiagramOpts -> Diagram Canvas R2 -> BC.Context -> IO ()
canvasDia opts d context = do
  BC.send context $
    renderDia 
      Canvas 
      (CanvasOptions 
        (mkSizeSpec 
          (fromIntegral <$> width opts) 
          (fromIntegral <$> height opts))) 
    d

multiMain :: [(String, Diagram Canvas R2)] -> IO ()
multiMain ds = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog True)
  case selection opts of
    Nothing  -> putStrLn "No diagram selected."
    Just sel -> case lookup sel ds of
      Nothing -> putStrLn $ "Unknown diagram: " ++ sel
      Just d  -> canvasRender opts d