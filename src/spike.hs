module Main where

import Graphics.UI.Gtk.WebKit.WebFrame 
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebNavigationAction

import Graphics.UI.Gtk

import Data.IORef

import Control.Applicative

import Control.Monad.Trans

{-

Links:
- http://trac.webkit.org/wiki/WebKitGTK
- ...

-}

instance Show NavigationReason where    
    show WebNavigationReasonLinkClicked	     = "WebNavigationReasonLinkClicked"
    show WebNavigationReasonFormSubmitted    = "WebNavigationReasonFormSubmitted"
    show WebNavigationReasonBackForward	     = "WebNavigationReasonBackForward"
    show WebNavigationReasonReload	     = "WebNavigationReasonReload"
    show WebNavigationReasonFormResubmitted  = "WebNavigationReasonFormResubmitted"
    show WebNavigationReasonOther	     = "WebNavigationReasonOther"

-- | install listeners for various signals
hookupWebView web = do
  on web downloadRequested $ \ down -> do
         uri <- downloadGetUri down
         print ("Download uri",uri)
         return True

  let foo str webFr netReq webNavAct _webPolDec = do
                      print str
                      print =<< webFrameGetName webFr
                      print =<< webFrameGetUri webFr
                      print =<< networkRequestGetUri netReq
                      print =<< webNavigationActionGetReason webNavAct
                      --print =<< webNavigationActionGetTargetFrame webNavAct
                      --print (str,t1,t2,t3,t4,t5)
                      return False

  on web navigationPolicyDecisionRequested $ foo "navigationPolicyDecisionRequested"

--  on web newWindowPolicyDecisionRequested $ foo "newWindowPolicyDecisionRequested"


page = do
  -- webkit widget
  web <- webViewNew
  webViewSetTransparent web True
  let loadHome = webViewLoadUri web "http://google.com"
  loadHome
  webViewSetMaintainsBackForwardList web False
--  print =<< widgetGetSizeRequest web
--  widgetSetSizeRequest web 1240 1024
--  print =<< widgetGetSizeRequest web
  
  -- plugins are causing trouble. disable them.
  settings <- webViewGetWebSettings web
  set settings [webSettingsEnablePlugins := False]
      
  hookupWebView web

  -- scrolled window to enclose the webkit
  scrollWeb <- scrolledWindowNew Nothing Nothing
  containerAdd scrollWeb web

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
  containerAdd page' scrollWeb
  
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