module GlobalVariables where

import Data.Global
import Datatypes

import Data.IORef
import Control.Concurrent.STM
import Control.Concurrent

-- | stores the whole browse tree state
browseTreeVar :: BrowseTreeState
browseTreeVar = declareTVar "spike::global::browseTree" []

-- | current page
currentPageVar :: TVar Page
currentPageVar = declareTVar "spike::global::currentPage" (error "current page not yet set")

-- | page counter -- for pgIdent
pageIdentVar :: TVar Int
pageIdentVar = declareTVar "spike::global::pageIdentCounter" 0