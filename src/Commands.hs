{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}

module Commands where

import Datatypes

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Global
import Data.IORef
import Data.Typeable

-- | commands channel. base implementations only do some logging, they need to be replaced with proper implementations when appropriate.
commandChannelVar :: IORef CommandControl
commandChannelVar = declareIORef "spike::global::commandChannel" emptyCommandControl

data CommandControl = CommandControl { sendCommand_ :: SomeCommand -> IO ()
                                     , registerListener_ :: (SomeCommand -> IO ()) -> IO () 
                                     } deriving Typeable

data SomeCommand = forall e . Command e => SomeCommand e deriving Typeable

instance Show SomeCommand where
    showsPrec p (SomeCommand e) = showsPrec p e

-- | Command class. Design copied after System.Exception.Exception class
class (Typeable c, Show c) => Command c where
    toCommand :: c -> SomeCommand
    fromCommand :: SomeCommand -> Maybe c

    toCommand = SomeCommand
    fromCommand (SomeCommand c) = Data.Typeable.cast c

sendCommand :: Command c => c -> IO ()
sendCommand c = do
  cc <- readIORef commandChannelVar
  sendCommand_ cc (toCommand c)

registerListener :: (Command c) => (c -> IO ()) -> IO ()
registerListener l = do
  cc <- readIORef commandChannelVar
  registerListener_ cc (\ c -> case fromCommand c of
                                 Nothing -> return ()
                                 Just x -> l x)

emptyCommandControl = CommandControl
                       (\ _ -> print "(default) sendCommand: command ignored")
                       (\ _ -> print "(default) registerListener: listener ignored")

setupCommandControl :: IO ()
setupCommandControl = do
  commandChan <- newChan
  listeners <- newTVarIO []

  let send c = writeChan commandChan c      
      register l = atomically $ do
                     ls <- readTVar listeners
                     writeTVar listeners (l:ls)

      handleCommand c = do
        ls <- readTVarIO listeners
        let logException c e = putStrLn $ "Exception while handling command: " ++ show c ++ " " ++ show (e :: SomeException)
        mapM_ (\ handler -> forkIO (Control.Exception.catch (handler c) (logException c))) ls

  forkIO $ do
    commands <- getChanContents commandChan
    mapM_ handleCommand commands

  let cc = CommandControl send register
  writeIORef commandChannelVar cc
  return ()

-- commands
data ViewPageCommand = ViewPageCommand PageID deriving (Typeable, Show) -- which page
data NewTopLevelPageCommand = NewTopLevelPageCommand URL deriving (Typeable, Show) -- url
data NewChildPageCommand = NewChildPageCommand PageID URL deriving (Typeable, Show) -- parent, url
data ClosePageCommand = ClosePageCommand PageID deriving (Typeable, Show) -- which page

instance Command ViewPageCommand where
instance Command NewChildPageCommand where
instance Command NewTopLevelPageCommand where
instance Command ClosePageCommand where