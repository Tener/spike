{-# LANGUAGE PackageImports #-}
module Main where

import Graphics.UI.Gtk.WebKit.WebFrame 
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import Graphics.UI.Gtk.WebKit.WebWindowFeatures

import Graphics.UI.Gtk

import Data.IORef

import "mtl" Control.Monad.Trans

import Data.Tree as Tree

{-

Links:
- http://trac.webkit.org/wiki/WebKitGTK
- ...

-}

data BrowseNode = BrowseNode { widget :: WebView,
                               identifier :: Int }

instance Show NavigationReason where    
    show WebNavigationReasonLinkClicked	     = "WebNavigationReasonLinkClicked"
    show WebNavigationReasonFormSubmitted    = "WebNavigationReasonFormSubmitted"
    show WebNavigationReasonBackForward	     = "WebNavigationReasonBackForward"
    show WebNavigationReasonReload	     = "WebNavigationReasonReload"
    show WebNavigationReasonFormResubmitted  = "WebNavigationReasonFormResubmitted"
    show WebNavigationReasonOther	     = "WebNavigationReasonOther"

-- | install listeners for various signals
hookupWebView :: WebViewClass object => object -> IO (a, WebView) -> IO ()
hookupWebView web newPage = do
  on web downloadRequested $ \ down -> do
         uri <- downloadGetUri down
         print ("Download uri",uri)
         return True

  let foo str webFr netReq webNavAct _webPolDec = do
                      t0 <- return str
                      t1 <- webFrameGetName webFr
                      t2 <- webFrameGetUri webFr
                      t3 <- networkRequestGetUri netReq
                      t4 <- webNavigationActionGetReason webNavAct
                      print (t0,t1,t2,t3,t4)
                      return False

  on web newWindowPolicyDecisionRequested $ foo "newWindowPolicyDecisionRequested"
  on web createWebView $ \ _ -> print "XXX: hookupWebView: createWebView" >> fmap snd newPage
  on web downloadRequested $ \ _ -> print "downloadRequested" >> return False

  return ()


page :: IO (a, WebView) -> IO (VPaned, WebView)
page newPage = do
  -- webkit widget
  web <- webViewNew
  webViewSetTransparent web True
  let loadHome = webViewLoadUri web "http://google.com"
  loadHome
  webViewSetMaintainsBackForwardList web False
  
  -- plugins are causing trouble. disable them.
  settings <- webViewGetWebSettings web
  set settings [webSettingsEnablePlugins := False]
      
  hookupWebView web newPage

  -- debug features
  feats <- webViewGetWindowFeatures web
  let printX x y = print (x,y)
  printX "new page: webWindowFeaturesFullscreen"         =<< get feats webWindowFeaturesFullscreen
  printX "new page: webWindowFeaturesHeight"             =<< get feats webWindowFeaturesHeight
  printX "new page: webWindowFeaturesWidth"              =<< get feats webWindowFeaturesWidth
  printX "new page: webWindowFeaturesX"                  =<< get feats webWindowFeaturesX
  printX "new page: webWindowFeaturesY"                  =<< get feats webWindowFeaturesY
  printX "new page: webWindowFeaturesLocationbarVisible" =<< get feats webWindowFeaturesLocationbarVisible
  printX "new page: webWindowFeaturesMenubarVisible"     =<< get feats webWindowFeaturesMenubarVisible
  printX "new page: webWindowFeaturesScrollbarVisible"   =<< get feats webWindowFeaturesScrollbarVisible
  printX "new page: webWindowFeaturesStatusbarVisible"   =<< get feats webWindowFeaturesStatusbarVisible
  printX "new page: webWindowFeaturesToolbarVisible"     =<< get feats webWindowFeaturesToolbarVisible
  
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
  page' <- vPanedNew
  containerAdd page' menu
  containerAdd page' scrollWeb
  
  widgetShowAll page'
  return (page',web)

notebook :: IO Notebook
notebook = do
  nb <- notebookNew
  cnt <- newIORef (1 :: Int)
  let newPage = do
         (p,w) <- page newPage
         cnt' <- readIORef cnt
         writeIORef cnt (cnt' + 1)
         ix <- notebookAppendPage nb p ("Page: " ++ show cnt')  
         widgetShowAll nb
         print ("newPage",ix)
         return (p,w)
  newPage
  
  let cb str = do
         mods <- eventModifier
         key <- eventKeyVal
         name <- eventKeyName
         let ev = (key,name,mods)
         liftIO $ print (str,ev)
         case ev of
           (_,"t",[Control]) -> liftIO (print str >> newPage) >> return True
           _ -> return False
  
  on nb keyPressEvent $ cb "keyPress"
  on nb keyReleaseEvent $ cb "onKeyRelease"
  
  return nb

treeview :: IO Frame
treeview = do
  let mkTree sub x = Node x sub
      sub1 = map (mkTree []) [1,2,3]
      sub2 = map (mkTree []) [4,5,6]
      sub3 = map (mkTree []) [7,8,9]
      t = zipWith mkTree [sub1,sub2,sub3] [10,20,30]
  model <- treeStoreNew (t :: [Tree Int])
  tv <- treeViewNewWithModel model
  treeViewSetHeadersVisible tv True
  col <- treeViewColumnNew
  treeViewColumnSetTitle col "Int column"
  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer True
  cellLayoutSetAttributes col renderer model $ \row -> [ cellText := show row ]
  treeViewAppendColumn tv col
  -- frame with label
  tv' <- frameNew
  containerAdd tv' tv
  frameSetLabel tv' "TreeView"
  
  widgetSetSizeRequest tv' (-1) 100
  widgetSetSizeRequest tv (-1) 100

  return tv'

main :: IO ()
main = do
  -- initialize
  initGUI

  -- notebook for web views
  nb <- notebook

  -- tree view widget
  tv <- treeview
  
  -- assemble the page
  whole <- vPanedNew
  containerAdd whole tv
  containerAdd whole nb

  print ("divider:")
  print =<< panedGetPosition whole
  panedSetPosition whole 100
  

  -- show all, enter loop
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 10,
               windowTitle := "window 1",
               containerChild := whole,
               windowAllowGrow := True ]
  widgetShowAll window

  mainGUI
  return ()
