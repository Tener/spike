module Main where

import Graphics.UI.Gtk

import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebNavigationAction

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import Data.IORef
import Data.Maybe
import Data.Tree as Tree
import Data.Tree.Zipper

import System.Directory
import System.Exit
import System.FilePath
import System.IO.Unsafe
import System.Process
import System.Random

import Text.Printf

import qualified Data.Foldable as F
import qualified Data.List
import qualified Data.Traversable as T

import CFunctions
import Commands
import Datatypes
import GlobalVariables
import NotebookSimple
import Utils
import VisualBrowseTree
import BrowseTreeOperations

-- notebook synchronization
noEntriesInBox :: ContainerClass self => self -> IO ()
noEntriesInBox box = do
  containerForeach box (containerRemove box)
  label <- labelNew Nothing
  labelSetMarkup label "<span color=\"#909090\">(no elements here)</span>"
  containerAdd box label
  widgetShowAll box

--------------
setupGlobals = do
  appDir <- getAppUserDataDirectory "Spike"
  createDirectoryIfMissing False appDir

  spikeSetupWebkitGlobals (appDir </> "spike.webkit.db") (appDir </> "spike.webkit.cookie.db")

main :: IO ()
main = do
 -- experimental
 setupCommandControl

 -- init gui & webkit
 initGUI
 setupGlobals
 
 -- glue together gui. yuck.
 parentsBox <- hBoxNew False 1   :: IO HBox

 viewPageRef <- newIORef undefined
 siblingsNotebookSimple <- notebookSimpleNew (\p -> do { fun <- readIORef viewPageRef; fun p}) :: IO NotebookSimple
 childrenBox <- hBoxNew False 1  :: IO HBox

 widgetSetSizeRequest parentsBox (-1) 40
 widgetSetSizeRequest childrenBox (-1) 40

 inside <- vBoxNew False 1
 (\sw -> boxPackStart inside sw PackNatural 1) =<< newScrolledWindowWithViewPort parentsBox
 boxPackStart inside (ns_widget siblingsNotebookSimple) PackGrow 1
 (\sw -> boxPackStart inside sw PackNatural 1) =<< newScrolledWindowWithViewPort childrenBox

 -- global state
 -- currentPage <- newTVarIO (error "current page is undefined for now...")
 -- btreeVar <- newTVarIO []
 let currentPage = GlobalVariables.currentPageVar
     btreeVar = GlobalVariables.browseTreeVar

 -- define refresh layout and others

 let viewPage :: Page -> IO ()
     viewPage page = do
       print "CALL: viewPage"
       atomically $ writeTVar currentPage page
       refreshLayout
   
     refreshLayout = do
       print "CALL: refreshLayout"

       btree <- readTVarIO btreeVar
       page <- readTVarIO currentPage
       let (parents,siblings,children) = getPageSurrounds btree page

       viewPagesSimpleNotebook siblings siblingsNotebookSimple
       notebookSimpleSelectPage siblingsNotebookSimple page

       case parents of
         [] -> noEntriesInBox parentsBox
         parents' -> listPagesBox viewPage parents' parentsBox    -- update parents
       case children of
         [] -> noEntriesInBox childrenBox
         children' -> listPagesBox viewPage children' childrenBox  -- update children


     spawnHomepage = do
         let homepage = "https://google.com"
         page <- newTopPage btreeVar refreshLayout homepage
         atomically $ writeTVar currentPage page
         notebookSimpleSelectPage siblingsNotebookSimple page
         viewPage page
         return ()

 -- experimental: command listeners
 registerListener (\ (ViewPageCommand pid) -> 
                       do
                         p <- findPageByID pid
                         case p of
                           Nothing -> return ()
                           Just pg -> viewPage pg)

 registerListener (\ (NewTopLevelPageCommand url) -> newTopPage btreeVar refreshLayout url >> return ())

 writeIORef viewPageRef viewPage

 -- create root page
 spawnHomepage
 newTopPage btreeVar refreshLayout "http://news.google.com"

 -- show main window
 window <- windowNew
 onDestroy window mainQuit
 set window [ containerBorderWidth := 10,
              windowTitle := "Spike browser",
              containerChild := inside,
              windowAllowGrow := True ]
 widgetShowAll window

 -- tree view window
 visualBrowseTreeWindow viewPage btreeVar

 -- refresh layout once and run GTK loop
 refreshLayout
 mainGUI

 return ()
