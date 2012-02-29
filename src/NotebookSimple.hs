{-# LANGUAGE ViewPatterns #-}

-- this module is a simple reimplementation of GTK Notebook widget.
-- I needed to be able to change the list of pages without receving a signal from it.
module NotebookSimple where

import Graphics.UI.Gtk
import Control.Concurrent
import Control.Concurrent.STM

import Data.List (elemIndex)

import Utils
import Datatypes

data NotebookSimple = NotebookSimple { ns_tabs :: HBox
                                     , ns_widget :: Widget
                                     , ns_pages :: TVar [Page]
                                     , ns_currentPage :: TVar (Maybe Int)
                                     , ns_refresh :: IO ()
                                     }

newScrolledWindowWithViewPort :: WidgetClass child => child -> IO ScrolledWindow
newScrolledWindowWithViewPort child = do
  sw <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
  scrolledWindowAddWithViewport sw child
  return sw

notebookSimpleNew :: (Page -> IO ()) -> IO NotebookSimple
notebookSimpleNew focusOnPage = do
  buttonsBox <- hBoxNew False 1
  tabsBox <- hBoxNew False 1
  vbox <- vBoxNew False 1

  widgetSetSizeRequest buttonsBox (-1) 30

  (\sw -> boxPackStart vbox sw PackNatural 1) =<< newScrolledWindowWithViewPort buttonsBox
  boxPackStart vbox tabsBox PackGrow 1

--  containerAdd vbox buttonsBox
--  containerAdd vbox tabsBox

  widgetShowAll vbox

  pgs <- newTVarIO []
  curr <- newTVarIO Nothing

  let updateButtons = postGUIAsync $ do

         pages <- readTVarIO pgs
         listPagesBox focusOnPage pages buttonsBox 

         -- on b buttonActivated $ atomically $ writeTVar curr

--          containerForeach buttonsBox (containerRemove buttonsBox)
--          mapM_ (\ pg -> do
--  
--                   
--  
--                   l <- labelNew (Just title) -- TODO: use AccelLabel and shortcuts for specific pages
--                   labelSetEllipsize l EllipsizeEnd
--                   labelSetWidthChars l 20
--                   labelSetSingleLineMode l True
--         
--                   b <- buttonNew
--                   containerAdd b l
--         
--                   boxPackStart buttonsBox b PackNatural 1
--                   
--                   
--                   -- TODO: tutaj powinniśmy pisać zmienną w modelu drzewiastym a nie zmienną z modelu notebooka. ewentualnie można to jakoś powiązać, byle z sensem...
--                   --       zasadniczo przepływ powinien być w jedną tylko stronę, nie powinno być pętli wyzwalaczy
--                   on b buttonActivated $ atomically $ writeTVar curr (Just i) 
--                ) pages
--  
--          widgetShowAll buttonsBox

      updatePage = do 
        wg <- getCurrentWidget
        case wg of
          Just wg' -> postGUIAsync (do
                                     containerForeach tabsBox (containerRemove tabsBox)
                                     set tabsBox [ containerChild := (snd wg') ] >> widgetShowAll vbox)
          Nothing -> print "strange stuff."

      getCurrentWidget = do
         c <- readTVarIO curr
         p <- readTVarIO pgs
         case c of
           Nothing -> return Nothing
           Just c' -> return (if c' < length p then Just (p !! c') else Nothing)

  let watchdog page = do
         page' <- waitTVarChangeFrom page curr
         updatePage
         watchdog page'

  let watchdog2 pages = do
         pages' <- waitTVarChangeFrom pages pgs
         updateButtons
         watchdog2 pages'

  forkIO (watchdog2 =<< readTVarIO pgs)
  forkIO (watchdog =<< readTVarIO curr)

  let updateAll = updateButtons >> updatePage

  return (NotebookSimple { ns_tabs = tabsBox, ns_widget = (toWidget vbox), ns_pages = pgs, ns_currentPage = curr, ns_refresh = updateAll })

notebookSimpleAddPage :: NotebookSimple -> Page -> IO ()
notebookSimpleAddPage ns@(ns_pages -> pages) page = do
  atomically $ do
    pages' <- readTVar pages
    writeTVar pages (pages' ++ [page])

  notebookSimpleSelectPageIfNone ns

notebookSimpleSelectPage :: NotebookSimple -> Page -> IO ()
notebookSimpleSelectPage ns page = do
  atomically $ do
    current <- readTVar (ns_currentPage ns)
    pages <- readTVar (ns_pages ns)
    writeTVar (ns_currentPage ns) (elemIndex page pages)
  

notebookSimpleSelectPageIfNone :: NotebookSimple -> IO ()
notebookSimpleSelectPageIfNone ns = do
  atomically $ do
    current <- readTVar (ns_currentPage ns)
    case current of
      Nothing -> writeTVar (ns_currentPage ns) (Just 0)
      Just _ -> return ()

