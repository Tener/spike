{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}

module Datatypes where

import Control.Concurrent.STM
import Data.Tree
import Data.Typeable
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import Graphics.UI.Gtk.WebKit.WebView

data Page = Page { pgWeb :: WebView
                 , pgWidget :: Widget
                 , pgIdent :: Int
                 , pgStartURI :: String }
          deriving Typeable
          

type PageID = Int
type PageLink = String

type URL = String

instance Eq Page where
    p1 == p2 = pgWidget p1 == pgWidget p2

type BrowseTree = Forest Page
type BrowseTreeState = TVar BrowseTree

-- | wrapper newtype to avoid orphan instances
newtype MyShow a = MyShow a
instance Show (MyShow NavigationReason) where
    show (MyShow WebNavigationReasonLinkClicked    ) = "WebNavigationReasonLinkClicked"
    show (MyShow WebNavigationReasonFormSubmitted  ) = "WebNavigationReasonFormSubmitted"
    show (MyShow WebNavigationReasonBackForward    ) = "WebNavigationReasonBackForward"
    show (MyShow WebNavigationReasonReload         ) = "WebNavigationReasonReload"
    show (MyShow WebNavigationReasonFormResubmitted) = "WebNavigationReasonFormResubmitted"
    show (MyShow WebNavigationReasonOther          ) = "WebNavigationReasonOther"

-- | wrapper type for things that doesn't really have Show instance defined, like functions
data DontShow a = DontShow { fromDontShow :: a } deriving Typeable
instance Show (DontShow a) where
    showsPrec p _ = showsPrec p "DontShow {contents are hidden}"
