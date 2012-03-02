module VisualBrowseTree where



import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebNavigationAction
-- import Graphics.UI.Gtk.WebKit.WebWindowFeatures

import System.IO.Unsafe
import System.Process
import System.Exit

import Graphics.UI.Gtk
import Text.Printf
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent

import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Tree.Zipper
import Data.Maybe
import qualified Data.List

import Utils
import NotebookSimple
import Datatypes

import Data.Tree as Tree


browseTreeToSVG :: [Tree Page] -> IO String
browseTreeToSVG btree = do
  let ellipsis t n | length t < n = t
                   | otherwise = take n t ++ "..."

  nodeID <- newTVarIO (0::Int)
  btree' <- mapM (T.mapM (\p -> do
            i <- atomically $ do
                   iden <- readTVar nodeID
                   writeTVar nodeID (iden+1)
                   return iden
            t <- getPageTitle p
            return (i,t))) btree

  let prelude = unlines ["digraph \"Browse tree\" {",
                         "graph [",
                         "fontname = \"Helvetica-Oblique\",",
                         "page = 10",
                         "size = 30",
                         " ];"]
      -- labels = unlines [ printf "d%d [label=\"%s\"];" i t | (i,t) <- concatMap flatten btree' ]
      footer = "}"

      btreeZip = concatMap flattenToZipper' btree' -- ellipsis t 15
      labels = unlines [ printf "d%d [URL=\"http://google.com\", shape=polygon, fixedsize=true, fontsize=8, width=1.25, height=0.25, tooltip=\"%s\", label=\"%s\"];"
                                i t (ellipsis t 10) | (i,t) <- map label btreeZip ]
      edges = unlines [ printf "d%d -> d%d;" (fst . label . fromJust . parent $ z ) (fst . label $ z)
                            | z <- btreeZip,
                              parent z /= Nothing]

      edges2 =  [ ((fst . label . fromJust . parent $ z ),(fst . label $ z))
                      | z <- btreeZip,
                             parent z /= Nothing]


      everything = prelude ++ edges ++ labels ++ footer

  print everything
  print edges2
  tot@(code,svg,dotErr) <- readProcessWithExitCode "dot" ["-Tsvg"] everything
 -- _ <- readProcessWithExitCode "dot" ["-Tsvg","-ograph.svg"] everything
 -- _ <- readProcessWithExitCode "dot" ["-ograph.dot"] everything
  print tot
  case code of
    ExitSuccess -> return svg
    ExitFailure c -> return $ printf "<text>Error running 'dot' command. Exit code: %s\n%s</text>" (show c) dotErr


-- visualBrowseTreeWidget :: t -> IO Widget
visualBrowseTreeWidget :: TVar [Tree Page] -> IO Widget
visualBrowseTreeWidget btreeVar = do
  -- webkit widget
  web <- webViewNew
  webViewSetTransparent web True
  webViewSetFullContentZoom web True

  -- scrolled window to enclose the webkit
  scrollWeb <- scrolledWindowNew Nothing Nothing
  containerAdd scrollWeb web

  settings <- webViewGetWebSettings web
  set settings [webSettingsEnablePlugins := False]

  let refreshSVG = do
        svg <- browseTreeToSVG =<< readTVarIO btreeVar
        webViewLoadString web svg (Just "image/svg+xml") Nothing ""

  on web navigationPolicyDecisionRequested $ \ webframe networkReq webNavAct webPolDec -> do
    print "[navigationPolicyDecisionRequested]"
    muri <- networkRequestGetUri networkReq
    case muri of
      Nothing -> return ()
      Just uri -> print ("visualBrowseTreeWidget",uri)

    return True

  -- watch btreeVar for changes, update
  let watchdog page = do
          page' <- waitTVarChangeFrom page btreeVar
          postGUIAsync refreshSVG
          watchdog page'
  forkIO (watchdog =<< readTVarIO btreeVar)
  forkIO (forever $ do
            threadDelay (10^6)
            postGUIAsync refreshSVG)

  refreshSVG
  return (toWidget scrollWeb)

visualBrowseTreeWindow btreeVar = do
  window <- windowNew
  visualBT <- visualBrowseTreeWidget btreeVar
  set window [ containerBorderWidth := 10,
              windowTitle := "Spike browser - visual browse tree",
              containerChild := visualBT,
              windowAllowGrow := True ]
  widgetShowAll window

  return window