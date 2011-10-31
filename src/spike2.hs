{-# LANGUAGE PackageImports, FlexibleInstances #-}
module Main where

import Graphics.UI.Gtk.WebKit.WebFrame 
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebNavigationAction
-- import Graphics.UI.Gtk.WebKit.WebWindowFeatures

import Graphics.UI.Gtk

-- import Control.Concurrent
import Control.Concurrent.STM

-- import "mtl" Control.Monad.Trans

import Data.Tree as Tree

data History = Hist { hiNow :: String, hiPrev :: [String], hiNext :: [String] }

data Page = Page { pgWeb :: WebView, pgWidget :: Widget, pgHistory :: TVar History }

type BrowseTree = Forest Page
type BrowseTreeState = TVar BrowseTree


-- helpers

-- wrapper newtype to avoid orphan instances
newtype MyShow a = MyShow a
instance Show (MyShow NavigationReason) where	
    show (MyShow WebNavigationReasonLinkClicked	   )  = "WebNavigationReasonLinkClicked"
    show (MyShow WebNavigationReasonFormSubmitted  )  = "WebNavigationReasonFormSubmitted"
    show (MyShow WebNavigationReasonBackForward	   )  = "WebNavigationReasonBackForward"
    show (MyShow WebNavigationReasonReload	   )  = "WebNavigationReasonReload"
    show (MyShow WebNavigationReasonFormResubmitted)  = "WebNavigationReasonFormResubmitted"
    show (MyShow WebNavigationReasonOther	   )  = "WebNavigationReasonOther"


isWidgetPage :: Widget -> Page -> IO Bool
isWidgetPage w p = do
  return (pgWidget p == w)

isSamePage :: Page -> Page -> Bool
isSamePage p1 p2 = pgWidget p1 == pgWidget p2

getPageTitle :: Page -> IO String
getPageTitle p = do
  mtit <- webViewGetTitle (pgWeb p)
  h <- readTVarIO (pgHistory p)
  case mtit of
    Nothing -> return (hiNow h)
    Just t -> return t

-- operations on single page
-- | install listeners for various signals
hookupWebView :: WebViewClass object => object -> IO WebView -> IO ()
hookupWebView web createSubPage = do
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
                      print (t0,t1,t2,t3,MyShow t4)
                      return False

  on web newWindowPolicyDecisionRequested $ foo "newWindowPolicyDecisionRequested"
-- on web navigationPolicyDecisionRequested ...
  on web createWebView $ \ _ -> print "createWebView" >> createSubPage
  -- on web downloadRequested $ \ _ -> print "downloadRequested" >> return False

  -- JavaScript stuff: TODO
 -- scriptAlert
 -- scriptConfirm
 -- scriptPrompt
 -- printRequested
 -- statusBarTextChanged
  on web consoleMessage $ \ s1 s2 i s3 -> print ("[JavaScript/Console message]: ",s1,s2,i,s3) >> return False
 -- closeWebView
 -- titleChanged

  return ()

newWeb :: BrowseTreeState -> String -> IO (Widget, WebView)
newWeb btreeSt url = do
  -- webkit widget
  web <- webViewNew
  webViewSetTransparent web True
  let loadHome = webViewLoadUri web url
  loadHome
  webViewSetMaintainsBackForwardList web False -- TODO: or maybe True?

  -- plugins are causing trouble. disable them.
  settings <- webViewGetWebSettings web
  set settings [webSettingsEnablePlugins := False]

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
  page <- vPanedNew
  containerAdd page menu
  containerAdd page scrollWeb

  widgetShowAll page

  let widget = toWidget page
      ww = (widget,web)
      newChildPage = do
        btree <- readTVarIO btreeSt
        case findPageWidget btree widget of
          Just p -> do
            p' <- newPage ww "about:blank" -- TODO: win the battle over the power to navigate the web view.
            let btree' = addChild btree p p'
            atomically $ writeTVar btreeSt btree'
            return (pgWeb p')
          Nothing -> do
            error "findPageWidget returned Nothing, can't provide a new window"
        
  hookupWebView web newChildPage

  return ww

-- todo: refactor historyMove{Forward,Backward}
historyMoveForward :: Page -> IO ()
historyMoveForward page = do
  url <- atomically $ do
    hist <- readTVar (pgHistory page)
    case hiNext hist of
      [] -> return Nothing
      (x:xs) -> do
        let h' = Hist x (hiNow hist : hiPrev hist) xs
        writeTVar (pgHistory page) h'
        return (Just x)
 
  case url of
    Just url' -> webViewLoadUri (pgWeb page) url'
    Nothing -> return ()

historyMoveBackward :: Page -> IO ()
historyMoveBackward page = do
  url <- atomically $ do
    hist <- readTVar (pgHistory page)
    case hiPrev hist of
      [] -> return Nothing
      (x:xs) -> do
        let h' = Hist x xs (hiNow hist : hiNext hist)
        writeTVar (pgHistory page) h'
        return (Just x)
 
  case url of
    Just url' -> webViewLoadUri (pgWeb page) url'
    Nothing -> return ()

-- move to new page, discard any hiNext out there
navigateToPage :: Page -> String -> IO ()
navigateToPage page url = do
  atomically $ do
    hist <- readTVar (pgHistory page)
    let h' = Hist url (hiNow hist : hiPrev hist) []
    writeTVar (pgHistory page) h'
 
  webViewLoadUri (pgWeb page) url

-- operations on tree

-- new browse tree. contains single page with home page.
-- newBrowseTree :: IO BrowseTreeState
newBrowseTree :: IO (BrowseTreeState, Page)
newBrowseTree = do
  let homepage = "https://google.com"
  btvar <- newTVarIO []
  ww@(_widget,_web) <- newWeb btvar homepage 
  tp <- newLeafURL ww homepage
  -- note: a minor race condition. 
  -- consider a case when homepage contains javascript that opens a new (sub)page. 
  -- this can be fired before the following lines, 
  -- in which case this new page get's lost, 
  -- because findPageWidget will return Nothing.
  -- this will in turn (currently) cause application crash, due to unhandled 'error'.

  atomically $ writeTVar btvar [tp]
  return (btvar, rootLabel tp)

newLeafURL :: (Widget, WebView) -> String -> IO (Tree Page)
newLeafURL ww url = do
  page <- (newPage ww url)
  return (newLeaf page)

newPage :: (Widget, WebView) -> String -> IO Page
newPage (widget,webv) url = do
  hist <- newTVarIO (Hist url [] [])
  return (Page { pgWidget=widget, pgWeb=webv, pgHistory=hist})

newLeaf :: Page -> Tree Page
newLeaf page = Node page []

addChild :: BrowseTree -> Page -> Page -> BrowseTree
addChild btree parent child = let 
    aux (Node page sub) | isSamePage page parent = Node page (sub ++ [newLeaf child])
                        | otherwise              = Node page (map aux sub)
    in map aux btree

findPageWidget :: BrowseTree -> Widget -> Maybe Page
findPageWidget btree w = let fun p = pgWidget p == w
                             flat = concatMap flatten btree
                             filt = filter fun flat
                         in
                           case filt of
                             [] -> Nothing
                             (x:_) -> Just x

-- notebook synchronization

-- | view selected pages in notebook. clears existing pages first.
viewPagesNotebook :: [Page] -> Notebook -> IO ()
viewPagesNotebook pages nb = do
  -- remove pages we don't need
  current <- (zip [0..]) `fmap` containerGetChildren nb
  let aux _ [] = return ()
      aux n ((i,w):ws) = do
         ok <- mapM (isWidgetPage w) pages
         if any id ok then aux n ws else notebookRemovePage nb (i-n) >> aux (n+1) ws
  aux 0 current

  -- add missing pages
  -- (notebookAppendPage nb)
  let addMissing p = do
        let w = pgWidget p
        n <- notebookPageNum nb w
        case n of
          Nothing -> getPageTitle p >>= notebookAppendPage nb w >> return ()
          Just _ -> return ()

  mapM_ addMissing pages 
  -- reorder everything to get right order
  let fixOrder (i,p) = do
        notebookReorderChild nb (pgWidget p) i

  mapM_ fixOrder (zip [0..] pages)


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
           boxPackStart box l PackNatural 1
                        
           on l focus $ \ direction -> do
             print ("SIGNAL: on focus",direction, title)
             focusOnPage p
             return False
        ) pages
  return ()

main :: IO ()
main = do
 initGUI

 parentsBox <- hBoxNew False 1
 siblingsNotebook <- notebookNew
 centralBox <- frameNew
 childrenBox <- hBoxNew False 1

 inside <- vBoxNew False 1
 containerAdd inside parentsBox 
 containerAdd inside siblingsNotebook 
 containerAdd inside centralBox
 containerAdd inside childrenBox 

 (_btree,topPage) <- newBrowseTree

-- let parentsBox = undefined :: HBox
--     siblingsNotebook = undefined :: Notebook
--     centralBox = undefined :: Frame
--     childrenBox = undefined :: HBox

 let viewPage :: Page -> IO ()
     viewPage page = do
       listPagesBox viewPage [] parentsBox   -- update parents
       viewPagesNotebook [] siblingsNotebook -- update siblings
       showPage page centralBox              -- show webview inside central box
       listPagesBox viewPage [] childrenBox  -- update children
     
     showPage p box = do
         containerForeach box (containerRemove box)
         containerAdd box (pgWidget p)

 viewPage topPage

 -- show all, enter loop
 window <- windowNew
 onDestroy window mainQuit
 set window [ containerBorderWidth := 10,
              windowTitle := "window 1",
              containerChild := inside,
              windowAllowGrow := True ]
 widgetShowAll window

 mainGUI
 return ()
