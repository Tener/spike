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
  (wx,wy) <- widgetGetSizeRequest child
  widgetSetSizeRequest sw (wx+5) (wy+5)
  return sw

notebookSimpleNew :: (Page -> IO ()) -> IO NotebookSimple
notebookSimpleNew focusOnPage = do
  buttonsBox <- hBoxNew False 1
  contentBox <- hBoxNew False 1
  vbox <- vBoxNew False 1

  widgetSetSizeRequest buttonsBox (-1) 40

  (\sw -> boxPackStart vbox sw PackNatural 1) =<< newScrolledWindowWithViewPort buttonsBox
  boxPackStart vbox contentBox PackGrow 1

  widgetShowAll vbox

  pgs <- newTVarIO [] :: IO (TVar [Page])
  curr <- newTVarIO Nothing

  let updateButtons = postGUIAsync $ do
         pages <- readTVarIO pgs
         listPagesBox focusOnPage pages buttonsBox 

      updatePage = do 
        wg <- getCurrentWidget
        case wg of
          Just wg' -> postGUIAsync (do
                                     containerForeach contentBox (containerRemove contentBox)
                                     set contentBox [ containerChild := (pgWidget wg') ] >> widgetShowAll vbox)
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
  return (NotebookSimple { ns_tabs = contentBox
                         , ns_widget = (toWidget vbox)
                         , ns_pages = pgs
                         , ns_currentPage = curr
                         , ns_refresh = updateAll })

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


-- | list pages in a box
-- listPagesBox :: (BoxClass box) => [Page] -> box -> IO ()
listPagesBox :: BoxClass box => (Page -> IO a) -> [Page] -> box -> IO ()
listPagesBox focusOnPage pages box = do
  containerForeach box (containerRemove box)
  mapM_ (\p -> do
           title <- getPageTitle p
           l <- labelNew (Just title) -- TODO: use AccelLabel and shortcuts for specific pages
           labelSetEllipsize l EllipsizeEnd
           labelSetWidthChars l 20
           labelSetSingleLineMode l True

           b <- buttonNew
           containerAdd b l

           boxPackStart box b PackNatural 1

           on b buttonActivated $ do
             print ("SIGNAL: on focus", title)
             focusOnPage p
             return ()
        ) pages
  widgetShowAll box
  return ()
