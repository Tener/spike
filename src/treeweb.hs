module Main where

import Graphics.UI.Gtk.WebKit.WebFrame 
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk

import Data.IORef

import Control.Monad.Trans

{-

Links:
- http://trac.webkit.org/wiki/WebKitGTK
- ...

-}

page = do
  -- webkit widget
  web <- webViewNew
  webViewSetTransparent web True
  let loadHome = webViewLoadUri web "http://google.com"
  loadHome
  webViewSetMaintainsBackForwardList web False
  print =<< widgetGetSizeRequest web
  widgetSetSizeRequest web 1240 1024
  print =<< widgetGetSizeRequest web
  
  -- plugins are causing trouble. disable them.
  settings <- webViewGetWebSettings web
  set settings [webSettingsEnablePlugins := False]
  
  -- menu
  menu <- hBoxNew False 1
  quit <- buttonNewWithLabel "Quit"
  reload <- buttonNewWithLabel "Reload"
  on reload buttonActivated $ webViewReload web 
  goHome <- buttonNewWithLabel "Home"
  on goHome buttonActivated $ loadHome
  containerAdd menu quit
  containerAdd menu reload
  containerAdd menu goHome

  -- fill the page
  page' <- vBoxNew False 10
  containerAdd page' menu
  containerAdd page' web
  
  widgetShowAll page'
  return page'

main :: IO ()
main = do
  -- inicjalizacja
  initGUI

  -- notatnik na instancje web view
  nb <- notebookNew
  cnt <- newIORef (1 :: Int)
  let newPage = do
         p <- page
         cnt' <- readIORef cnt
         writeIORef cnt (cnt' + 1)
         ix <- notebookAppendPage nb p ("Page: " ++ show cnt')  
         widgetShowAll nb
         print ix
  newPage

  let cb = do
         mods <- eventModifier
         key <- eventKeyVal
         name <- eventKeyName
         let ev = (key,name,mods)
         liftIO $ print ev
         case ev of
           (_,"t",[Control]) -> liftIO newPage >> return True
           _ -> return False
  on nb keyPressEvent $ cb
  on nb keyReleaseEvent $ cb

  -- pokazujemy wszystko i zapadamy w pętle
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 10,
               containerChild := nb,
               windowAllowGrow := True ]
  widgetShowAll window
  mainGUI

  return ()