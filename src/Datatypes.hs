{-# LANGUAGE PackageImports, FlexibleInstances, DoRec, ForeignFunctionInterface #-}
module Datatypes where

import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import System.IO.Unsafe
import Graphics.UI.Gtk
import Control.Concurrent.STM
import Data.Tree

data History = Hist { hiNow :: String, hiPrev :: [String], hiNext :: [String] } deriving Show

data Page = Page { pgWeb :: WebView, pgWidget :: Widget, pgHistory :: TVar History }

instance Show Page where
    show pg = "Page { pgWeb=?, pgWidget=?, pgHistory=" ++ show (unsafeDupablePerformIO (readTVarIO (pgHistory pg))) ++ "}"

instance Eq Page where
    p1 == p2 = pgWidget p1 == pgWidget p2

type BrowseTree = Forest Page
type BrowseTreeState = TVar BrowseTree

-- wrapper newtype to avoid orphan instances
newtype MyShow a = MyShow a
instance Show (MyShow NavigationReason) where
    show (MyShow WebNavigationReasonLinkClicked    )  = "WebNavigationReasonLinkClicked"
    show (MyShow WebNavigationReasonFormSubmitted  )  = "WebNavigationReasonFormSubmitted"
    show (MyShow WebNavigationReasonBackForward    )  = "WebNavigationReasonBackForward"
    show (MyShow WebNavigationReasonReload         )  = "WebNavigationReasonReload"
    show (MyShow WebNavigationReasonFormResubmitted)  = "WebNavigationReasonFormResubmitted"
    show (MyShow WebNavigationReasonOther          )  = "WebNavigationReasonOther"
